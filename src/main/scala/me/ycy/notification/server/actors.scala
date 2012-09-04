package me.ycy.notification.server

import akka.actor._
import akka.util.duration._

import org.mashupbots.socko.events._
import org.mashupbots.socko.handlers._

import java.util.{UUID, Date}

import org.jboss.netty.channel.Channel

import me.ycy.notification.api._
import dispatch.json._

case class WsClient(
  name: String,
  event: WebSocketHandshakeEvent
)

case class WsFrame(event: WebSocketFrameEvent)
object CheckConnectivity

class ClientActor extends Actor with ActorLogging {
  var clients: Map[String, Channel] = Map()

  // generate unique client name
  def genName(name: String): String = {
    val PostfixMax = 100000
    var newName = name

    if (clients.contains(name)) {
      var i = 1
      while (clients.contains(name + "_" + i) && i <= PostfixMax) {
        i += 1
      }
      newName = {
        if (i > PostfixMax)
          name + "_" + UUID.randomUUID
        else
          name + "_" + i
      }
    }

    newName
  }

  // check status of client with name
  def checkClient(name: String): Unit = {
    if (!clients.contains(name)) return;

    val ch = clients(name)
    if (!ch.isConnected) {
      log.debug("client {} disconnected, removed.", name)
      clients -= name
      context.stop(context.actorFor(name))
    }
  }

  self ! CheckConnectivity

  def receive = {
    case WsClient(name, event) ⇒ {
      checkClient(name)

      val newName = genName(name)
      if (newName != name) {
        log.info("client with name {} already exist, rename to {}",
          name, newName)
      }
      log.debug("new client {} connected.", newName)

      clients += newName → event.channel

      val b = context.actorOf(Props[WebSocketBroadcaster], newName)
      b ! WebSocketBroadcasterRegistration(event)
    }

    case WsFrame(event) ⇒ {
      if (event.isText) {
        log.debug("get text " + event.readText)

        try {
          val js = Js(event.readText)
          val cmd = Command.fromJson(js).get

          val na = context.actorFor("/user/notification")

          // send to notificationActor
          cmd match {
            case c: CreateCommand ⇒
              clients.find(_._2 == event.channel) match {
                case Some((name, _)) ⇒
                  na ! c.copy(client = name)
                case None ⇒
                  na ! c
              }
            case _ ⇒
              na ! cmd
          }
        }
        catch {
          case e ⇒ e.printStackTrace // tell client of invalid command
        }
      }
      else {
        // TODO: warn,
      }
    }

    case CheckConnectivity ⇒ {
      for (n ← clients.keys) { checkClient(n) }
      context.system.scheduler.scheduleOnce(
        2 seconds, self, CheckConnectivity
      )
    }

  }
}

object NotificationActor {
  // check timeout for notification with uuid
  // if timestamp of the notification is eq to timestamp,
  // then the notification is updated and this check should skip.
  case class CheckTimeout(uuid: UUID, timestamp: Date)
}

class NotificationActor extends Actor with ActorLogging {
  import NotificationActor._

  var map: Map[UUID, Notification] = Map()

  val k = "notification.timeout"
  val timeout0 = context.system.settings.config.getLong(k)
  def timeout(t: Long) =
    if (t == 0)
      timeout0 milliseconds
    else
      t milliseconds

  def receive = {
    case cmd: Command ⇒ processCommand(cmd)
    case CheckTimeout(uuid, timestamp) ⇒ checkTimeout(uuid, timestamp)
  }

  def processCommand(cmd: Command): Unit = {
      log.debug("get command {}", cmd)

      cmd match {
        case cc: CreateCommand ⇒ {
          if (map.contains(cmd.uuid)) {
            log.warning("receive command with same uuid: {}", cmd.uuid)
            return
          }

          map += cc.uuid → Notification(
            client = cc.client,
            uuid = cc.uuid,
            title = cc.title,
            body = cc.body,
            notificationClass = cc.notificationClass,
            titleClass = cc.titleClass,
            bodyClass = cc.bodyClass,
            timestamp = cc.timestamp,
            timeout = cc.timeout
          )

          if (cc.timeout >= 0) {
            context.system.scheduler.scheduleOnce(
              timeout(cc.timeout),
              self, CheckTimeout(cc.uuid, cc.timestamp)
            )
          }
        }

        case uc: UpdateCommand ⇒ {
          if (!map.contains(uc.uuid)) {
            log.debug("skip update for non exist notification {}", uc.uuid)
            return
          }

          var n = map(uc.uuid).copy(timestamp = uc.timestamp)
          if (uc.title.isDefined) { n = n.copy(title = uc.title.get) }
          if (uc.body.isDefined) { n = n.copy(body = uc.body.get) }
          if (uc.notificationClass.isDefined) {
            n = n.copy(notificationClass = uc.notificationClass.get)
          }
          if (uc.titleClass.isDefined) {
            n = n.copy(titleClass = uc.titleClass.get)
          }
          if (uc.bodyClass.isDefined) {
            n = n.copy(bodyClass = uc.bodyClass.get)
          }
          if (uc.timeout.isDefined) {
            n = n.copy(timeout = uc.timeout.get)
          }

          map += uc.uuid → n

          if (n.timeout >= 0) {
            context.system.scheduler.scheduleOnce(
              timeout(n.timeout),
              self, CheckTimeout(n.uuid, n.timestamp)
            )
          }
        }

        case xc: CloseCommand ⇒ {
          log.debug("close notification {}", xc.uuid)
          map -= xc.uuid
        }
      }

    val json = cmd.toJson.toString
      context.actorFor("/user/ui") ! WebSocketBroadcastText(json)
  }


  def checkTimeout(uuid: UUID, timestamp: Date): Unit = {
      // return when the notification to check is not exist
    if (!map.contains(uuid)) {
      log.debug("skip check timeout for non exist notification {}", uuid)
      return
    }

    if (map(uuid).timestamp != timestamp) {
      log.debug("skip check timeout for updated notification {} -> {}",
        timestamp, map(uuid).timestamp)
      return
    }

    self ! CloseCommand(uuid, new Date())
  }

}

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
  case class CheckTimeout(uuid: UUID, timestamp: Date, pause: Long)

  // new ui connected, with associated WebSocketBroadcaster
  // after send active notifications, stop the WebSocketBroadcaster.
  case class UIConnected(b: WebSocketBroadcaster)
}

class NotificationActor extends Actor with ActorLogging {
  import NotificationActor._

  var map: Map[UUID, Notification] = Map()
  var pausedTime: Long = 0 // unit seconds

  val k = "notification.server.timeout"
  val timeout0 = context.system.settings.config.getLong(k)
  def timeout(t: Long) =
    if (t < 0)
      timeout0 milliseconds
    else
      t milliseconds

  def receive = {
    case cmd: Command ⇒ processCommand(cmd)
    case event: Event ⇒ processEvent(event)
    case CheckTimeout(id, ts, p) ⇒ checkTimeout(id, ts, p)
  }

  def processCommand(cmd: Command): Unit = {
      log.debug("get command {}", cmd)

      cmd match {
        case cc: CreateCommand ⇒ {
          if (map.contains(cmd.uuid)) {
            log.warning("receive command with same uuid: {}", cmd.uuid)
            return
          }

          map += cc.uuid → Notification.create(cc)

          if (cc.timeout != Command.TimeoutNever) {
            context.system.scheduler.scheduleOnce(
              timeout(cc.timeout),
              self, CheckTimeout(cc.uuid, cc.timestamp, pausedTime)
            )
          }
        }

        case uc: UpdateCommand ⇒ {
          if (!map.contains(uc.uuid)) {
            log.debug("skip update for non exist notification {}", uc.uuid)
            return
          }

          var n = map(uc.uuid).update(uc)
          map += uc.uuid → n

          if (n.timeout != Command.TimeoutNever) {
            context.system.scheduler.scheduleOnce(
              timeout(n.timeout),
              self, CheckTimeout(n.uuid, n.timestamp, pausedTime)
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

  def processEvent(event: Event) = event match {
    case FocusedEvent ⇒ {
      pausedTime += 1
    }
    case _ ⇒
  }


  def checkTimeout(uuid: UUID, timestamp: Date, p0: Long): Unit = {
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

    if (p0 == pausedTime) {
      self ! CloseCommand(uuid, new Date(), Command.Expired)
    }
    else {
      context.system.scheduler.scheduleOnce(
        (pausedTime - p0) seconds,
        self, CheckTimeout(uuid, timestamp, pausedTime)
      )
    }
  }

}

package me.ycy.notification.server

import akka.actor._
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

import org.mashupbots.socko.events._
import org.mashupbots.socko.handlers._

import java.util.{UUID, Date}

import org.jboss.netty.channel.Channel

import me.ycy.notification.api._
import dispatch.classic.json._

// language feature
import scala.language.postfixOps

case class WsClient(
  name: String,
  event: WebSocketHandshakeEvent
)

case class WsFrame(event: WebSocketFrameEvent)
object CheckConnectivity

// server side client event
case class SClientEvent(event: ClientEvent, client: String)

class ClientActor extends Actor with ActorLogging {
  var clients: Map[String, Channel] = Map()

  // generate unique client name
  def genName(name: String): String = {
    val PostfixMax = 100000
    var newName = name

    if (clients.contains(name) || !context.actorFor(name).isTerminated) {
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
          // tell client of invalid command
          case e: Throwable ⇒ e.printStackTrace
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

    case SClientEvent(e, client) ⇒ {
      log.debug("get client event {} for {}", e, client)
      if (clients.contains(client)) {
        context.actorFor(client) ! WebSocketBroadcastText(e.toJson.toString)
      }
      else {
        log.debug("event target {} not exist, skip", client)
      }
    }
  }
}

object NotificationLifetimeUpdater {
  case class Tick(time: Long);
  // every pause object cause pause 1.5 second
  case object Pause;
}

class NotificationLifetimeUpdater extends Actor with ActorLogging {
  import NotificationLifetimeUpdater._

  var uuid: UUID = null
  var lifetime = -1L
  var keep = false
  var pause = false
  var pCnt = 0L // number of pause number

  def scheduleTickAndUpdateUI() = {
    var time = lifetime % 1000
    if (time == 0) time = 1000

    if (!keep) {
      context.parent ! StatusCommand(uuid = uuid, lifetime = lifetime)
    }

    context.system.scheduler.scheduleOnce(
      time milliseconds,
      self, Tick(time)
    )
  }

  def receive = {
    // create command
    case (id: UUID, lt: Long, k: Boolean) ⇒ {
      uuid = id
      lifetime = lt
      keep = k

      scheduleTickAndUpdateUI()
    }

    // update command
    case (up: Long, k: Boolean) ⇒ {
      lifetime = up
      keep = k
    }

    case Tick(time) ⇒ {
      if (!pause) lifetime -= time


      if ((lifetime > 0) || keep) {
        scheduleTickAndUpdateUI()
      }
      else {
        // send CloseCommand & stop self
        context.parent ! CloseCommand(uuid = uuid, reason = Command.Expired)
        context.stop(self)
      }
    }

    case Pause ⇒ {
      pCnt += 1
      val p = pCnt // create a snapshot
      pause = true
      context.system.scheduler.scheduleOnce(1.5 seconds) {
        if (this.pCnt == p) this.pause = false
      }
    }
  }
}

object NotificationActor {
  // new ui connected, with associated WebSocketBroadcaster
  // after send active notifications, stop the WebSocketBroadcaster.
  case class UIConnected(b: ActorRef)
}

class NotificationActor extends Actor with ActorLogging {
  import NotificationActor._

  var map: Map[UUID, Notification] = Map()

  val k = "notification.server.timeout"
  val timeout0 = context.system.settings.config.getLong(k)
  def timeout(t: Long) =
    if (t < 0)
      timeout0
    else
      t

  def receive = {
    case cmd: Command ⇒ processCommand(cmd)
    case event: Event ⇒ processEvent(event)
    case UIConnected(b) ⇒ uploadNotification(b)
  }

  def uploadNotification(b: ActorRef) = {
    for (n ← map.values) {
        b ! WebSocketBroadcastText(n.toCommand.toJson.toString)
    }
    context.system.scheduler.scheduleOnce(1 seconds) {
      context.stop(b)
    }
  }

  def processCommand(cmd: Command): Unit = {
    log.debug("get command {}, from {}", cmd, sender.path)

    cmd match {
      case cc: CreateCommand ⇒ {
        if (map.contains(cmd.uuid)) {
          log.warning("receive command with same uuid: {}", cmd.uuid)
          return
        }

        map += cc.uuid → Notification.create(cc, sender.path.toString)

        val lu = context.actorOf(
          Props[NotificationLifetimeUpdater],
          cc.uuid.toString
        )

        lu ! (
          cc.uuid,
          timeout(cc.timeout),
          cc.timeout == Command.TimeoutNever
        )
      }

      case uc: UpdateCommand ⇒ {
        if (!map.contains(uc.uuid)) {
          log.debug("skip update for non exist notification {}", uc.uuid)
          return
        }

        var n = map(uc.uuid).update(uc)
        map += uc.uuid → n

        val lu = context.actorFor(n.uuid.toString)
        lu ! (timeout(n.timeout), n.timeout == Command.TimeoutNever)
      }

      case xc: CloseCommand ⇒ {
        log.debug("close notification {}", xc.uuid)
        // send event
        map.get(xc.uuid) match {
          case None ⇒ // already closed
          case Some(n) ⇒ {
            val xe = ClosedEvent(uuid = xc.uuid, reason = xc.reason)
            context.actorFor(n.src) ! SClientEvent(xe, n.client)
            // stop lifetime updater
            context.stop(context.actorFor(xc.uuid.toString))
          }
        }
        // remove
        map -= xc.uuid
      }

      case _: StatusCommand ⇒
    }

    val json = cmd.toJson.toString
    context.actorFor("/user/ui") ! WebSocketBroadcastText(json)
  }

  def processEvent(event: Event): Unit = event match {
    case FocusedEvent ⇒ {
      for (id ← map.keys) {
        context.actorFor(id.toString) ! NotificationLifetimeUpdater.Pause
      }
    }

    case e: ClientEvent ⇒ {
      if (!map.contains(e.uuid)) {
        // this may happen is close command is explicitly called.
        log.info("get event {}, but notification {} not exist, skip",
          e, e.uuid)
        return
      }

      val n = map(e.uuid)
      context.actorFor(n.src) ! SClientEvent(e, n.client)

      if (e.isInstanceOf[ClosedEvent]) {
        map -= e.uuid
        // send to ui, ensure all ui close this notification
        val xe = e.asInstanceOf[ClosedEvent]
        val js = CloseCommand(uuid = xe.uuid, reason = xe.reason).toJson
        context.actorFor("/user/ui") ! WebSocketBroadcastText(js.toString)
      }
    }
  }

}

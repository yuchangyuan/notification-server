package me.ycy.notification.server

import org.freedesktop.Notifications
import org.freedesktop.Quad
import org.freedesktop.dbus._

import java.util.{List ⇒ JList, Map ⇒ JMap, ArrayList}
import java.util.UUID
import java.net.URL

import akka.actor._
import akka.util.duration._

class DBusBridge extends Actor with ActorLogging {
  import DBusBridge._

  var conn: DBusConnection = null

  context.actorOf(Props[NotificationService], "notification")

  //  ------------- before connected to dbus -------------
  def receive = {
    case Connect(bus) ⇒ {
      try {
        if (bus.size == 0) {
          conn = DBusConnection.getConnection(DBusConnection.SESSION)
        }
        else {
          conn = DBusConnection.getConnection(bus)
        }

        log.info("connect to bus {} ok.", bus)

        // change handler
        context.become(process)
        // expose object
        context.actorFor("notification") ! RegisterOn(conn)
        // requset name
        self ! RequestName
      }
      catch {
        case e ⇒ log.warning("connect to bus {} failed", bus)
      }
    }

    case IsConnected ⇒ sender ! false
  }


  //  ------------ already connected to dbus -------------
  def process: Receive = {
    case Disconnect ⇒ {
      conn.disconnect()
      conn = null
      context.become(receive)
    }

    case IsConnected ⇒ sender ! true

    case RequestName ⇒ {
      try {
        conn.requestBusName(BusName)
        log.info("request bus name {} ok", BusName)
      }
      catch {
        case _ ⇒
          log.warning("request bus name {} failed, retry later.", BusName)
          context.system.scheduler.scheduleOnce(5 seconds, self, RequestName)
      }
    }

    case m: Signal ⇒ {
      conn.sendSignal(m.signal())
    }
  }
}

object DBusBridge {
  val BusName = "org.freedesktop.Notifications"
  val ObjectPath = "/org/freedesktop/Notifications"
  val Iface = "org.freedesktop.Notifications"

  //  -------- for DBusBridge --------
  case class Connect(bus: String)
  object Disconnect
  object IsConnected
  object RequestName
  // message & dbus signal
  trait Signal {
    def signal(): DBusSignal
  }

  case class NotificationClosed(id: Int, reason: Int) extends Signal {
    def signal() = new DBusSignal(
      null, ObjectPath, Iface, "NotificationClosed", "uu",
      new UInt32(id), new UInt32(reason)
    )
  }

  case class ActionInvoked(id: Int, actionKey: String) extends Signal {
    def signal() = new DBusSignal(
      null, ObjectPath, Iface, "ActionInvoked", "us",
      new UInt32(id), actionKey
    )
  }

  //  --- for NotificationService ----
  case class RegisterOn(dbus: DBusConnection)
}

class NotificationService extends Actor with Notifications with ActorLogging {
  import DBusBridge._
  import me.ycy.notification.api._

  // uuid to dbus id map
  var map: Map[UUID, Int] = Map()
  // last used id
  var lastId: Int = 0

  //  ---------------- Notifications impl ----------------
  def GetServerInformation(): Quad[String, String, String, String] = {
    new Quad("Notification Server", "Yu Changyuan", "1.0.0", "1.2")
  }

  def GetCapabilities(): JList[String] = {
    val al = new ArrayList[String]();
    al.add("actions")
    al.add("body")
    al.add("body-hyperlinks")
    al.add("body-images")
    al.add("body-markup")
    al
  }

  def CloseNotification(id: UInt32) = {
    log.debug("dbus close notification #{}", id)
    map.find(_._2 == id.intValue) match {
      case None ⇒ {
        log.warning("Notification with id {} not exist, skip", id.intValue)
      }
      case Some((uuid, _)) ⇒ {
        val xc = CloseCommand(uuid = uuid, reason = 3)
        context.actorFor("/user/notification") ! xc
      }
    }
  }

  def Notify(
    app_name: String,
    id: UInt32,
    icon: String,
    summary: String,
    body: String,
    actions: JList[String],
    hints: JMap[String, Variant[_]],
    timeout: Int
  ): UInt32 = {
    var aMap = Map[String, String]()
    for (i ← 0 until actions.size() / 2) {
      aMap += actions.get(2 * i) → actions.get(2 * i + 1)
    }

    var cc = CreateCommand(
      title = summary,
      body = genBody(body, icon, aMap),
      client = app_name
    )

    if (hints.get("urgency") != null) {
      hints.get("urgency").getValue() match {
        case 0 ⇒ {
          cc = cc.copy(notificationClass = List("low-urgency"))
        }
        case 2 ⇒ {
          cc = cc.copy(
            notificationClass = List("critical-urgency"),
            timeout = Command.TimeoutNever
          )
        }
        case _ ⇒
      }
    }

    val cmd: Command = map.find(_._2 == id.intValue) match {
      case None ⇒ cc
      case Some((uuid, _)) ⇒ {
        // this is a update command
        UpdateCommand(
          uuid = uuid,
          title = Some(cc.title),
          body = Some(cc.body),
          notificationClass = Some(cc.notificationClass),
          timeout = Some(cc.timeout)
        )
      }
    }

    context.actorFor("/user/notification") ! cmd

    lastId += 1
    map += cc.uuid → lastId
    new UInt32(lastId)
  }


  def isRemote(): Boolean = false

  //  -------------------- actor impl --------------------
  def receive = {
    case RegisterOn(conn) ⇒ {
      conn.exportObject(ObjectPath, this)
      log.info("expose notification service.")
    }

    case SClientEvent(e: ClosedEvent, _) ⇒ {
      log.debug("get closed event {}", e)
      map.get(e.uuid).foreach(id ⇒
        context.parent ! NotificationClosed(id, e.reason)
      )
      map -= e.uuid // remove closed notification
    }

    case SClientEvent(e: ClickedEvent, _) ⇒ {
      log.debug("get clicked event {}", e)
      map.get(e.uuid).foreach(id ⇒
        context.parent ! ActionInvoked(id, e.id)
      )
    }
  }

  //  ------------------- other method -------------------
  def genBody(
    body: String,
    icon: String,
    actions: Map[String, String]
  ): String = {
    // URL extractor
    object IsURL {
      def unapply(url: String): Option[URL] = {
        try { Some(new URL(url)) }
        catch { case _ ⇒ None }
      }
    }

    // TODO
    val iconDiv = icon match {
      case "" ⇒ ""
      case IsURL(_) ⇒ {
        "<div style='width:40px;float:left;'><img src='" +
        icon +
        "' width='32px' height='32px'/></div>"
      }
      case str ⇒ {
        ""
      }
    }

    val actionDiv = "<div>" + actions.toList.map(kv ⇒
      "<button id='" + kv._1 +
      "'>" + kv._2 + "</button>"
    ).mkString("\n") +
    "</div>"

    val bodyDiv = "<div style='float:left;'>" +
      body.split("\n").map(
        "<p style='margin:0.1em'>" + _ + "</p>"
      ).mkString("\n") +
      actionDiv +
      "</div>"

    "<div>" +
    iconDiv + "\n" +
    bodyDiv + "</div>"
  }
}

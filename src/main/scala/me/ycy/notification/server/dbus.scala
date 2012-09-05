package me.ycy.notification.server

import org.freedesktop.Notifications
import org.freedesktop.Quad
import org.freedesktop.dbus._

import java.util.{List ⇒ JList, Map ⇒ JMap, ArrayList}
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
  }
}

object DBusBridge {
  val BusName = "org.freedesktop.Notifications"
  val ObjectPath = "/org/freedesktop/Notifications"

  //  -------- for DBusBridge --------
  case class Connect(bus: String)
  object Disconnect
  object IsConnected
  object RequestName

  //  --- for NotificationService ----
  case class RegisterOn(dbus: DBusConnection)
}

class NotificationService extends Actor with Notifications with ActorLogging {
  import DBusBridge._
  import me.ycy.notification.api._

  //  ---------------- Notifications impl ----------------
  def GetServerInformation(): Quad[String, String, String, String] = {
    new Quad("Notification Server", "Yu Changyuan", "1.0.0", "1.2")
  }

  def GetCapabilities(): JList[String] = {
    val al = new ArrayList[String]();
    al.add("body")
    al.add("body-hyperlinks")
    al.add("body-images")
    al.add("body-markup")
    al
  }

  def CloseNotification(id: UInt32) = {
    // TODO:

    log.debug("dbus close notification #{}", id)
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
    // TODO:

    var cc = CreateCommand(
      title = summary,
      body = "<pre>" + body + "</pre>",
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


    context.actorFor("/user/notification") ! cc

    // TODO: uuid to uint32 map
    new UInt32(0)
  }


  def isRemote(): Boolean = false

  //  -------------------- actor impl --------------------
  def receive = {
    case RegisterOn(conn) ⇒ {
      conn.exportObject(ObjectPath, this)
      log.info("expose notification service.")
    }
  }
}

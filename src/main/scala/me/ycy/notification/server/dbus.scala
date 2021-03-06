package me.ycy.notification.server

import org.freedesktop.Notifications
import org.freedesktop.Quad
import org.freedesktop.dbus._

import java.util.{List ⇒ JList, Map ⇒ JMap, ArrayList}
import java.util.UUID
import java.net.URL

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.language.postfixOps

import me.ycy.desktop.util.IconTheme
import org.apache.commons.codec.binary.Base64

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
        case e: Throwable ⇒ log.warning("connect to bus {} failed", bus)
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
        case _: Throwable ⇒
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
  import java.io.File

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

    val (title1, body1) = gen(summary, body, icon, aMap)

    var cc = CreateCommand(
      title = title1,
      body = body1,
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
            timeout =
              if (timeout == Command.TimeoutDefault)
                Command.TimeoutNever
              else
                timeout
          )
        }
        case _ ⇒
      }
    }

    log.debug("old map is {}", map)
    log.debug("id is {}", id)

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

    // default to input id
    var retId = id.intValue

    if (cmd.isInstanceOf[CreateCommand]) {
      lastId += 1
      map += cc.uuid → lastId
      retId = lastId
    }

    log.debug("new map is {}", map)

    new UInt32(retId)
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
  val iconTheme = IconTheme(
    context.system.settings.config.getString(
      "notification.server.icon-theme"
    ),
    "/usr/share/notify-osd/icons" :: _
  )
  val base64 = new Base64()

  def imgSrc(icon: String): String = {
    import java.io.{File, FileInputStream}
    iconTheme.findIcon(icon, 48) match {
      case Some(filename) ⇒
        // NOTE, here assume file has valid extension name
        val ext = filename.split("\\.").last
        val file = new File(filename)
        val ary = Array.ofDim[Byte](file.length.toInt)
        new FileInputStream(file).read(ary)
        // NOTE, here use commons-codec 1.5
        "data:image/" + ext + ";base64," + base64.encodeAsString(ary)
      case _ ⇒
        "icons/" + icon + ".svg"
    }
  }

  def gen(
    title: String,
    body: String,
    icon: String,
    actions: Map[String, String]
  ): (String, String) = {
    // URL extractor
    object IsURL {
      def unapply(url: String): Option[URL] = {
        try { Some(new URL(url)) }
        catch { case _: Throwable ⇒ None }
      }
    }

    val fileRegex = "^/.*".r

    def iconT(url: String) =
      <div style="width:48px;height:48px;float:left;position:relative;left:-0.5em;padding:0px;">
      <img src={url}
           style='width:48px;height:48px;' />
      </div>

    val iconDiv = icon match {
      case "" ⇒ ""
      case IsURL(_) ⇒ iconT(icon)
      case fileRegex() ⇒ iconT("file://" + icon)
      case _ ⇒ iconT(imgSrc(icon))
    }

    val r1 = iconDiv + title

    val actionDiv = <div>{
      actions.toList.map(kv ⇒
        <button id={kv._1}>{kv._2}</button>
      )}</div>

    val bodyDiv = "<div style=''>" +
      body.replaceAll("\n", "<br/>") +
      actionDiv +
      "</div>"

    val r2 =
      if (iconDiv == "")
        "<div>" + bodyDiv + "</div>"
      else
        "<div style='position:relative;left:-1em;min-height:0.5em;'>" +
        bodyDiv + "</div>"

    (r1, r2)
  }
}

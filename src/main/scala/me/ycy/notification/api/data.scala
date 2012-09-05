package me.ycy.notification.api

import java.util.UUID
import java.util.Date

import dispatch.json._

case class Notification(
  client: String,
  uuid: UUID,
  title: String,
  body: String,
  notificationClass: List[String],
  titleClass: List[String],
  bodyClass: List[String],
  timestamp: Date,
  timeout: Long
)

object Command {
  private def jsArray2strList(x: Option[JsValue]): Option[List[String]] =
    x.map {ary ⇒
      ary.asInstanceOf[JsArray].self.
      asInstanceOf[List[JsString]].map(_.self)
    }

  private def jsArray2strList1(x: Option[JsValue]): List[String] =
    jsArray2strList(x) match {
      case None ⇒ List()
      case Some(r) ⇒ r
    }

  private def jsNumber2date(n: JsValue): Date =
    new Date(n.asInstanceOf[JsNumber].self.toLong)

  def parseCreate(map: Map[String, JsValue]): Option[CreateCommand] = {
    try {
      Some(CreateCommand(
        client = map("client").asInstanceOf[JsString].self,
        title = map("title").asInstanceOf[JsString].self,
        body = map.getOrElse("body", JsString("")).
          asInstanceOf[JsString].self,
        notificationClass = jsArray2strList1(map.get("notification_class")),
        titleClass = jsArray2strList1(map.get("title_class")),
        bodyClass = jsArray2strList1(map.get("body_class")),
        timestamp = jsNumber2date(map("timestamp")),
        uuid = UUID.fromString(map("uuid").asInstanceOf[JsString].self),
        timeout = map.getOrElse("timeout", JsNumber(0)).
          asInstanceOf[JsNumber].self.toLong
      ))
    }
    catch {
      case e ⇒ e.printStackTrace; None
    }
  }

  def parseUpdate(map: Map[String, JsValue]): Option[UpdateCommand] = {
    try {
      Some(UpdateCommand(
        uuid = UUID.fromString(map("uuid").asInstanceOf[JsString].self),
        title = map.get("title").map(_.asInstanceOf[JsString].self),
        body = map.get("body").map(_.asInstanceOf[JsString].self),
        notificationClass = jsArray2strList(map.get("notification_class")),
        titleClass = jsArray2strList(map.get("title_class")),
        bodyClass = jsArray2strList(map.get("body_class")),
        timestamp = jsNumber2date(map("timestamp")),
        timeout = map.get("timeout").map(_.asInstanceOf[JsNumber].self.toLong)
      ))
    }
    catch {
      case e ⇒ e.printStackTrace; None
    }
  }

  def parseClose(map: Map[String, JsValue]): Option[CloseCommand] = {
    try {
      Some(CloseCommand(
        uuid = UUID.fromString(map("uuid").asInstanceOf[JsString].self),
        timestamp = jsNumber2date(map("timestamp")),
        reason = map.getOrElse("reason", JsNumber(Undefined)).
          asInstanceOf[JsNumber].self.toInt
      ))
    }
    catch {
      case _ ⇒ None
    }
  }


  def fromJson(js: JsValue): Option[Command] = js match {
    case JsObject(map0) ⇒ {
      val map = map0.map(kv ⇒ kv._1.self → kv._2)
      map.get("command") match {
        case Some(JsString("create")) ⇒ parseCreate(map)
        case Some(JsString("update")) ⇒ parseUpdate(map)
        case Some(JsString("close")) ⇒ parseClose(map)
        case _ ⇒ None
      }
    }
    case _ ⇒ None
  }

  val Expired = 1
  val Dismissed = 2
  val ExplicitlyClosed = 3
  val Undefined = 4
}

sealed trait Command {
  val command: String
  val uuid: UUID

  def toJson(): JsValue
}

case class CreateCommand(
  client: String,
  title: String,
  body: String = "",
  notificationClass: List[String] = List(),
  titleClass: List[String] = List(),
  bodyClass: List[String] = List(),
  timestamp: Date = new Date(),
  uuid: UUID = UUID.randomUUID,
  timeout: Long = 0
) extends Command {
  val command = "create"

  def toJson() = {
    var map: Map[JsString, JsValue] = Map(
      JsString("command") → JsString(command),
      JsString("client") → JsString(client),
      JsString("title") → JsString(title),
      JsString("notification_class") →
      JsArray(notificationClass.map(JsString.apply)),
      JsString("title_class") → JsArray(titleClass.map(JsString.apply)),
      JsString("body_class") → JsArray(bodyClass.map(JsString.apply)),
      JsString("timestamp") → JsNumber(timestamp.getTime),
      JsString("uuid") → JsString(uuid.toString),
      JsString("timeout") → JsNumber(timeout)
    )

    if (body.size > 0) map += JsString("body") → JsString(body)

    JsObject(map)
  }
}

case class UpdateCommand(
  uuid: UUID,
  title: Option[String] = None,
  body: Option[String] = None,
  notificationClass: Option[List[String]] = None,
  titleClass: Option[List[String]] = None,
  bodyClass: Option[List[String]] = None,
  timestamp: Date = new Date(),
  timeout: Option[Long] = None
) extends Command {
  val command = "update"

  def toJson() = {
    var map: Map[JsString, JsValue] = Map(
      JsString("command") → JsString(command),
      JsString("timestamp") → JsNumber(timestamp.getTime),
      JsString("uuid") → JsString(uuid.toString)
    )

    if (title.isDefined) map += JsString("title") → JsString(title.get)
    if (body.isDefined) map += JsString("body") → JsString(body.get)
    if (notificationClass.isDefined) {
      map += JsString("notification_class") →
        JsArray(notificationClass.get.map(JsString.apply))
    }
    if (titleClass.isDefined) {
      map += JsString("title_class") →
      JsArray(titleClass.get.map(JsString.apply))
    }
    if (bodyClass.isDefined) {
      map += JsString("body_class") →
      JsArray(bodyClass.get.map(JsString.apply))
    }
    if (timeout.isDefined) {
      map += JsString("timeout") → JsNumber(timeout.get)
    }

    JsObject(map)
  }
}

case class CloseCommand(
  uuid: UUID,
  timestamp: Date = new Date(),
  reason: Int = Command.Undefined
) extends Command {
  val command = "close"

  def toJson() = {
    var map: Map[JsString, JsValue] = Map(
      JsString("command") → JsString(command),
      JsString("timestamp") → JsNumber(timestamp.getTime),
      JsString("uuid") → JsString(uuid.toString),
      JsString("reason") → JsNumber(reason)
    )

    JsObject(map)
  }
}

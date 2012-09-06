package me.ycy.notification.server

import java.util.Date
import java.util.UUID

import me.ycy.notification.api._

object Notification {
  def create(cc: CreateCommand): Notification = {
    Notification(
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
  }
}

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
) {
  def update(uc: UpdateCommand): Notification = {
    var n = this.copy(timestamp = uc.timestamp)
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

    n
  }

  def toCommand(): CreateCommand = {
    CreateCommand(
      client = client,
      uuid = uuid,
      title = title,
      body = body,
      notificationClass = notificationClass,
      titleClass = titleClass,
      bodyClass = bodyClass,
      timestamp = timestamp,
      timeout = timeout
    )
  }
}

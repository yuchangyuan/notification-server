package me.ycy.notification.server

import java.util.Date
import java.util.UUID

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

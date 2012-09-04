package me.ycy.notification.server

import org.java_websocket.client.WebSocketClient
import org.java_websocket.handshake.ServerHandshake

import java.net.URI
import java.util.UUID

import me.ycy.notification.api._

object WS {
  def apply(name: String, host: String = "127.0.0.1", port: Int = 7755) = {
    val uri = "ws://" + host + ":" + port + "/c?name=" + name
    new WS(uri)
  }
}

class WS(uri: String) extends WebSocketClient(new URI(uri)) {
  def onMessage(msg: String): Unit = {
    println("onMessage " + msg)
  }

  def onOpen(h: ServerHandshake): Unit = {
    println("onOpen " + h)
  }

  def onClose(code: Int, reason: String, remote: Boolean): Unit = {
    println("onClose " + (code, reason, remote))
  }

  def onError(ex: Exception): Unit = {
    println("onError " + ex.toString)
  }

  def nCreate(title: String, body: String): UUID = {
    val cc = CreateCommand(client = "", title = title, body = body)
    send(cc.toJson.toString)
    cc.uuid
  }

  def nUpdate(uuid: UUID, title: Option[String], body: Option[String]) = {
    val uc = UpdateCommand(uuid = uuid, title = title, body = body)
    send(uc.toJson.toString)
  }

  def nClose(uuid: UUID) = {
    val cx = CloseCommand(uuid = uuid)
    send(cx.toJson.toString)
  }
}

package me.ycy.notification.server

import org.mashupbots.socko.webserver._
import akka.actor._
import org.mashupbots.socko.routes._
import org.mashupbots.socko.events._
import org.mashupbots.socko.handlers._

import org.jboss.netty.handler.codec.http.QueryStringDecoder

import me.ycy.notification.api._
import com.typesafe.config._
import java.io.File

class NotificationServer(val actorSystem: ActorSystem) {
  val cActor = actorSystem.actorOf(Props[ClientActor], "client")
  val nActor = actorSystem.actorOf(Props[NotificationActor], "notification")
  val uiActor = actorSystem.actorOf(Props[WebSocketBroadcaster], "ui")
  val dbusActor = actorSystem.actorOf(Props[DBusBridge], "dbus")

  val log = akka.event.Logging.getLogger(actorSystem, this)

  val routes = Routes({
    case HttpRequest(httpRequest) ⇒ httpRequest match {
      case _ ⇒ httpRequest.response.write(HttpResponseStatus.NOT_FOUND)
    }

    // UI
    case event @ Path("/") ⇒ event match {
      case event: WebSocketHandshakeEvent ⇒ {
        event.authorize(onComplete = Some(e ⇒ {
          // send to group broadcaster
          uiActor ! WebSocketBroadcasterRegistration(e)

          // send to notification actor
          val b = actorSystem.actorOf(Props[WebSocketBroadcaster])
          b ! WebSocketBroadcasterRegistration(e)
          nActor ! NotificationActor.UIConnected(b)

          log.info("ui connect")
        }))
      }
      case event: WebSocketFrameEvent ⇒ {
        log.debug("ui event")

        if (event.isText) {
          val json = event.readText
          log.debug("get event {}", json)
          // send to "/user/notification"
          Event(json) match {
            case Some(event) ⇒ nActor ! event
            case _ ⇒ log.info("event not valid.")
          }
        }
      }
    }

    // client
    case event @ Path("/c") ⇒ event match {
      case event: WebSocketHandshakeEvent ⇒ {
        // check, should contain name and length > 0
        val dec = new QueryStringDecoder(event.nettyHttpRequest.getUri)
        val params = dec.getParameters
        if (params.containsKey("name")) {
          val name = params.get("name").get(0)
          if (name.size > 0) {
            event.authorize(onComplete = Some(e ⇒ {
              log.info("client connected: {}", name)
              cActor ! WsClient(name, e)
            }))
          }
          else {
            log.info("client name is empty")
          }
        }
        else {
          log.info("missing client name")
        }
      }
      case event: WebSocketFrameEvent ⇒ {
        cActor ! WsFrame(event)
      }
    }
  })

}

object Main {
  def main(args: Array[String]) = {
    val configDefault = ConfigFactory.load()
    val config =
      if (args.size > 0)
        ConfigFactory.parseFile(new File(args(0))).withFallback(configDefault)
      else
        configDefault

    val actorSystem = ActorSystem("notification-server", config)

    val ns = new NotificationServer(actorSystem)

    // try to connect to session bus
    ns.dbusActor ! DBusBridge.Connect("")

    val port = config.getInt("notification.server.port")
    val host = config.getString("notification.server.host")
    val webServer = new WebServer(
      WebServerConfig(hostname = host, port = port),
      ns.routes, actorSystem
    )
    webServer.start()

    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run { webServer.stop() }
    })
  }

}

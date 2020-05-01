package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props
import com.typesafe.config.ConfigFactory

import scala.xml.Utility
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Play.current
import javax.inject._
import models.NodeActor.SetItUp
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport




object NodeActor {

  case object SetItUp

}


class NodeActor  @Inject() (configuration: Configuration, ws: WSClient) extends Actor with ActorLogging with InjectedActorSupport {
  val timeout = 10.seconds
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")

  var woAppActor: Option[ActorRef] = None

  def receive = LoggingReceive {
    // to the browser
    case SetItUp => {
      println("Set it up")
      if (woAppActor.isEmpty) {
        woAppActor = Some(context.actorOf(WOAppActor.props(ws), "woApp"))
        println("EOModel actor path " + woAppActor.get.path)
      }
    }
  }


}


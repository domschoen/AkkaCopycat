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
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport




object Workspace {
  def props(): Props = Props(new Workspace())

  case class Run(initialString: String, modifiedString: String, targetString: String)

}


class Workspace extends Actor with ActorLogging with InjectedActorSupport {
  import Workspace.Run
  var woAppActor: Option[ActorRef] = None

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString) => {
      println(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
    }
  }


}


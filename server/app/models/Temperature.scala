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




object Temperature {
  def props(): Props = Props(new Temperature())

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case class SetClamped(value: Boolean)
  case object GetClampTime
  case class ClampTime(value: Int)
  case object GetTemperature

  // To be implemented in actor asking for temperature
  case class TemperatureResponse(temperature: Double)
}


class Temperature extends Actor with ActorLogging with InjectedActorSupport {
  import Temperature._
  var woAppActor: Option[ActorRef] = None
  var clamp_time = 30
  var clamped = true
  var temperature: Double = 100.0

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
    }

    case GetTemperature =>
      sender ! TemperatureResponse(temperature)

    case SetClamped(value) =>
      clamped = value
    case GetClampTime =>
      sender() ! ClampTime(clamp_time)
  }


}


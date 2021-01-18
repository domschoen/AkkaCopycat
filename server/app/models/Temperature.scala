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

import scala.collection.mutable.ListBuffer




object Temperature {
  var clamp_time = 30

  def props(): Props = Props(new Temperature())

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case class SetClamped(value: Boolean)
  case object GetClampTime
  case class ClampTime(value: Int)
  case object GetTemperature
  case class SetTemperature(value: Double)

  case class Register(subscriber: ActorRef)

  // To be implemented in actor asking for temperature
  case class TemperatureResponse(temperature: Double)
  case class TemperatureChanged(temperature: Double)
  case class CheckClamped(codeletRuns: Int)
}


class Temperature extends Actor with ActorLogging with InjectedActorSupport {
  import Temperature._
  var woAppActor: Option[ActorRef] = None
  var clamped = true
  var temperature: Double = 100.0
  var subscribers = ListBuffer.empty[ActorRef]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString) => {
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
    }

    case Register =>
      subscribers += sender()
      //sender() ! TemperatureResponse(temperature)

    case SetTemperature(value) =>
      temperature = value
      subscribers.map(s => s ! TemperatureChanged(value))


    case GetTemperature =>
      sender ! TemperatureResponse(temperature)

    case SetClamped(value) =>
      clamped = value
    case GetClampTime =>
      sender() ! ClampTime(clamp_time)

    case CheckClamped(codeletRuns) =>
      if (codeletRuns >= clamp_time)
        clamped = false

  }


}


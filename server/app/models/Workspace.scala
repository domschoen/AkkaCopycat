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
  def props(slipnet: ActorRef): Props = Props(new Workspace(slipnet))

  case class Run(executionRun: ActorRef, initialString: String, modifiedString: String, targetString: String)
  case object Step
  case object Found
  case object ChooseRandomStructure
  case class BondWithNeighbor(temperature: Double)
}

abstract class WorkspaceStructure (
                           wString: WorkspaceString,
                           internalStrength : Double,
                           externalStrength: Double,
                           totalStrength: Double) {

}

class Bond (
 wString: WorkspaceString,
 internalStrength : Double,
 externalStrength: Double,
 totalStrength: Double
) extends WorkspaceStructure(wString, externalStrength, externalStrength, totalStrength) {

}





case class Correspondence (
                  wString: WorkspaceString,
                  internalStrength : Double,
                  externalStrength: Double,
                  totalStrength: Double
                ) extends WorkspaceStructure(wString, externalStrength, externalStrength, totalStrength)
case class Description (
                            wString: WorkspaceString,
                            internalStrength : Double,
                            externalStrength: Double,
                            totalStrength: Double
                          ) extends WorkspaceStructure(wString, externalStrength, externalStrength, totalStrength) {
  import models.SlipNode
  val descriptionType = Option.empty[SlipNode]
}
case class Replacement (
                            wString: WorkspaceString,
                            internalStrength : Double,
                            externalStrength: Double,
                            totalStrength: Double
                          ) extends WorkspaceStructure(wString, externalStrength, externalStrength, totalStrength)
case class Rule (
                            wString: WorkspaceString,
                            internalStrength : Double,
                            externalStrength: Double,
                            totalStrength: Double
                          ) extends WorkspaceStructure(wString, externalStrength, externalStrength, totalStrength)

abstract class WorkspaceObject (
  wString: WorkspaceString,
  internalStrength : Double,
  externalStrength: Double,
  totalStrength: Double) extends WorkspaceStructure(wString, externalStrength, externalStrength, totalStrength) {

  var intra_string_salience: Double = 0.0
  var inter_string_salience: Double = 0.0
  var total_salience: Double = 0.0
  var relative_importance: Double = 0.0

  var left_string_position: Int = 0
  var right_string_position: Int = 0

  var descriptions = List.empty[Description]


  def workspaceString() = wString

  override def toString(): String = {
    if (left_string_position==right_string_position)
      s"letter ($left_string_position)"
    else
      s"group ($left_string_position-$right_string_position)"
  }

}

class Group (
                   wString: WorkspaceString,
                   internalStrength : Double,
                   externalStrength: Double,
                   totalStrength: Double
                 ) extends WorkspaceObject(wString, externalStrength, externalStrength, totalStrength)

class Letter (
                   wString: WorkspaceString,
                   internalStrength : Double,
                   externalStrength: Double,
                   totalStrength: Double
                  ) extends WorkspaceObject(wString, externalStrength, externalStrength, totalStrength)


class WorkspaceString (s: String, x1: Int, y1: Int, x2: Int, y2: Int) {


  def length() = s.length

}


class Workspace(slipnet: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {
  import Workspace._
  import Slipnet._
  var executionRunActor: ActorRef = null
  var found_answer = false;
  var structures = List.empty[WorkspaceStructure]
  // initial ---> target
  // modified ----> ?
  var initial: WorkspaceString = null
  var target: WorkspaceString = null
  var modified: WorkspaceString = null


  def receive = LoggingReceive {

    case Found =>
      found_answer = true

    case Step =>
      if (found_answer) {
        executionRunActor ! ExecutionRun.Found
      } else {
        self ! Step
      }
    case ChooseRandomStructure =>
      val candidateStructures = structures.filter(s => s.isInstanceOf[Group] ||
        s.isInstanceOf[Bond] ||
        s.isInstanceOf[Correspondence]
      )
      if (candidateStructures.isEmpty) {
        log.debug("There are no structures built: fizzle")

      }
    case BondWithNeighbor(temperature) =>
      //          workspace_object fromob = workspace_formulas.choose_object("intra_string_salience",workspace.workspace_objects);
      val fromOpt = chooseObject(TemperatureAjustmentVariable.Intra_string_salience, temperature)
      fromOpt match {
        case None =>
          log.debug("BondWithNeighbor | failed with empty from")
        case Some(from) =>
          val toOpt = chooseNeighbor(from, temperature)
          toOpt match {
            case None =>
              log.debug("BondWithNeighbor | object has no neighbour - fizzle")
            case Some(to) =>
              log.debug(s"initial object chosen: $from in ${if (from.workspaceString() == initial) "initial" else "target"} string")
              log.debug(s"ito object: $to")

              slipnet ! BondFromTo(from, to)
          }
      }
  }



  def workspaceObjects(): List[WorkspaceObject] = structures.filter(s => s.isInstanceOf[WorkspaceObject]).asInstanceOf[List[WorkspaceObject]]

  def chooseObject(variable: String, temperature: Double) : Option[WorkspaceObject] = {
    val nonModifieds = workspaceObjects().filter(wo => wo.workspaceString != modified)
    chooseObjectFromList(nonModifieds, variable, temperature)
  }
  def chooseNeighbor(from: WorkspaceObject, temperature: Double) : Option[WorkspaceObject] = {
    val nonModifieds = workspaceObjects().filter(wo => (
      wo.workspaceString() == from.workspaceString()) &&
      (
        (wo.left_string_position == from.right_string_position + 1) ||
        (from.left_string_position== wo.right_string_position+1)
      )
    )
    chooseObjectFromList(nonModifieds, TemperatureAjustmentVariable.Intra_string_salience, temperature)
  }

  object TemperatureAjustmentVariable {
    val Intra_string_salience = "intra_string_salience"
    val Inter_string_salience = "inter_string_salience"
    val Total_salience = "total_salience"
    val Relative_importance = "relative_importance"
  }

  def chooseObjectFromList(list: List[WorkspaceObject], variable: String, temperature: Double): Option[WorkspaceObject] = {
    // chooses an object from the the list by a variable
    // eg "intra-string-salience" probabilistically adjusted for temperature
    if (list.isEmpty) {
      Option.empty[WorkspaceObject]
    } else {
      val adjustment: WorkspaceObject => Double = variable match {
        case TemperatureAjustmentVariable.Intra_string_salience => wo: WorkspaceObject => wo.intra_string_salience
        case TemperatureAjustmentVariable.Inter_string_salience => wo: WorkspaceObject => wo.inter_string_salience
        case TemperatureAjustmentVariable.Total_salience => wo: WorkspaceObject => wo.total_salience
        case TemperatureAjustmentVariable.Relative_importance => wo: WorkspaceObject => wo.relative_importance
      }
      val oProbs = list.map(wo => adjustment(wo))
      val index = Utilities.valueProportionalRandomIndexInValueList(oProbs)
      Option(list(index))
    }
  }


  def temperatureAdjustedValue(value: Double, temperature: Double) = Math.pow(value,((100.0-temperature)/30.0)+0.5)
}


package models

import akka.event.LoggingAdapter

import java.util.UUID
import models.WorkspaceStructure.WorkspaceStructureRep

object WorkspaceStructure {
  case class WorkspaceStructureRep(
                                  uuid :String
                                )

}


// Bond, Correspondence, Description, Replacement, Rule, WorkSpaceObject (Group, Letter)
abstract class WorkspaceStructure(log: LoggingAdapter) {
  var wString = Option.empty[WorkspaceString]

  val uuid = generateID()
  def generateID(): String = UUID.randomUUID().toString()
  var internal_strength = 0.0
  var external_strength = 0.0
  var total_strength = 0.0

  def workspaceString() = wString


  def workspaceStructureRep(): WorkspaceStructureRep = {
    WorkspaceStructureRep(uuid)
  }


  def update_strength_value(log: LoggingAdapter, activationBySlipNodeID: Map[String, Double], wos: List[WorkspaceObject]) = {
    calculate_internal_strength()
    calculate_external_strength(activationBySlipNodeID, wos)
    calculate_total_strength(log)
  };

  // See subclass implementation in Bond, Correspondence, Description, Rule, WorkSpaceObject (Group)
  def calculate_internal_strength() = {}
  def calculate_external_strength(activationBySlipNodeID: Map[String, Double], wos: List[WorkspaceObject]) = {}
  def calculate_total_strength(log: LoggingAdapter) = {
    total_strength = Formulas.weighted_average(internal_strength,
      internal_strength, external_strength, (100.0-internal_strength));
    printStrength(log);
  }

  def printStrength(log: LoggingAdapter): Unit = {
    if (log == null) {
      println(s"$uuid ${this.getClass} internal_strength $internal_strength external_strength $external_strength total strength: $total_strength")
    } else {
      log.debug(s"$uuid ${this.getClass} internal_strength $internal_strength external_strength $external_strength total strength: $total_strength")
    }
  }
  def total_weakness(): Double = {
    100.0-Math.pow(total_strength,0.95)
  }
}

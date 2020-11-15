package models

import java.util.UUID

// Bond, Correspondence, Description, Replacement, Rule, WorkSpaceObject (Group, Letter)
abstract class WorkspaceStructure {
  var wString = Option.empty[WorkspaceString]

  val uuid = generateID()
  def generateID(): String = UUID.randomUUID().toString()
  var internal_strength = 0.0
  var external_strength = 0.0
  var total_strength = 0.0

  def workspaceString() = wString


  def update_strength_value() = {
    calculate_internal_strength()
    calculate_external_strength()
    calculate_total_strength()
  };

  // implemented in Bond, Correspondence, Description, Rule, WorkSpaceObject (Group)
  def calculate_internal_strength() = {}
  def calculate_external_strength() = {}
  def calculate_total_strength() = {
    total_strength = Formulas.weighted_average(internal_strength,
      internal_strength, external_strength, (100.0-internal_strength));
    //System.out.println(this+" total strength:"+total_strength);
  }
  def total_weakness(): Double = {
    100.0-Math.pow(total_strength,0.95)
  }
}

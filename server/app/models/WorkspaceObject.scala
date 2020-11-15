package models

import akka.actor.ActorRef
import models.Description.DescriptionRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.WorkspaceStructureRep

import scala.collection.mutable.ListBuffer

abstract class WorkspaceObject(wString: WorkspaceString) extends WorkspaceStructure {


  var left_string_position: Int = 0
  var leftmost = false       // true if the object is the leftmost in the string

  var right_string_position: Int = 0
  var rightmost = false      // true if the object is the rightmost in the string

  var raw_importance: Double = 0.0
  var relative_importance: Double = 0.0
  var intra_string_happiness: Double = 0.0
  var intra_string_unhappiness: Double = 0.0
  var inter_string_happiness: Double = 0.0
  var inter_string_unhappiness: Double = 0.0
  var total_happiness: Double = 0.0
  var total_unhappiness: Double = 0.0
  var intra_string_salience: Double = 0.0
  var inter_string_salience: Double = 0.0
  var total_salience: Double = 0.0

  var descriptions = ListBuffer.empty[Description]
  var spans_string = false; // true if it spans the whole string

  var bonds = ListBuffer.empty[Bond]  // used in calculating intra string happiness
  // = number of bonds attached to the object
  var group = Option.empty[Group] // the group the object is a part of if it is in a group
  var correspondence = Option.empty[Correspondence] // if a Correspondence has been made
  var replacement = Option.empty[Replacement] // if a replacement has been made
  var changed = false    // true if it is the changed letter
  var new_answer_letter = false
  var clamp_salience = false
  var name = ""


  def middle_object(): Boolean = {
    // returns true if this is the middle object in the string
    var leftmost_neighbor = wString.objects.find(ob => ob.leftmost && (ob.right_string_position == left_string_position-1))
    var rightmost_neighbor = wString.objects.find(ob => ob.rightmost && (ob.left_string_position == right_string_position-1))

    leftmost_neighbor.isDefined && rightmost_neighbor.isDefined
  }


  def letter_span() = right_string_position - left_string_position + 1


  def relevant_descriptions(): List[Description] = {
    descriptions.toList.filter(d => d.descriptionType.activation==100.0)
  }


  override def toString(): String = {
    if (left_string_position==right_string_position)
      s"letter ($left_string_position)"
    else
      s"group ($left_string_position-$right_string_position)"
  }

  def descriptionReps(): List[DescriptionRep] = descriptions.toList.map(d => d.descriptionRep())
  def workspaceStructureRep(): WorkspaceStructureRep = {
    WorkspaceStructureRep(uuid,descriptionReps(),letterOrGroupCompanionReps(), spans_string, None)
  }

  def add_description(descriptionType: SlipNodeRep, descriptor: Option[SlipNodeRep]) = {
    val description = new Description(this, wString, descriptionType, descriptor)
    descriptions += description
  }



  def has_description(description: Description): Boolean = {
    descriptions.find(d =>
      description.descriptionType.id.equals(d.descriptionType.id) &&
        description.descriptor.isDefined &&
        d.descriptor.isDefined &&
        description.descriptor.get.equals(d.descriptor.get.id)
    ).isDefined
  }

  // What is not seen is that in found a description same dt, it can still return None (because descriptor is an option)
  def get_descriptor(dt: SlipNodeRep): Option[SlipNodeRep] = {
    val descriptionWithDescriptionType = descriptions.find(d => d.descriptionType.id == dt.id)
    descriptionWithDescriptionType match {
      case Some(d) => d.descriptor
      case None => None
    }
  }



  def letterOrGroupCompanions(): List[WorkspaceObject] = {
    workspaceString() match {
      case Some(ws) => ws.objects.filter(wo =>
        (wo != this) &&
          (isInstanceOf[Letter] && wo.isInstanceOf[Letter]) ||
          (isInstanceOf[Group] && wo.isInstanceOf[Group])
      ).toList
      case None => List.empty[WorkspaceObject]
    }
  }
  def letterOrGroupCompanionReps(): List[WorkspaceStructureRep] = letterOrGroupCompanions().map(_.workspaceStructureRep())


}

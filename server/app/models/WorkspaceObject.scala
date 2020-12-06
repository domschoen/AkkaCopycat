package models

import akka.actor.ActorRef
import akka.protobuf.DescriptorProtos.DescriptorProto
import models.Bond.BondRep
import models.Description.DescriptionRep
import models.Group.GroupRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.DescriptionTypeInstanceLinksToNodeInfo
import models.WorkspaceObject.WorkspaceObjectRep

import scala.collection.mutable.ListBuffer

object WorkspaceObject {
  case class WorkspaceObjectRep(
                                 uuid: String,
                                 descriptions: List[DescriptionRep],
                                 letterOrGroupCompanionReps: List[WorkspaceObjectRep],
                                 spans_string: Boolean,
                                 groupRep: Option[GroupRep],
                                 left_bond: Option[BondRep],
                                 right_bond: Option[BondRep]
                               )
}


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

  var left_bond = Option.empty[Bond]
  var right_bond = Option.empty[Bond]

  var bonds = ListBuffer.empty[Bond]  // used in calculating intra string happiness
  // = number of bonds attached to the object
  var group = Option.empty[Group] // the group the object is a part of if it is in a group
  var correspondence = Option.empty[Correspondence] // if a Correspondence has been made
  var replacement = Option.empty[Replacement] // if a replacement has been made
  var changed = false    // true if it is the changed letter
  var new_answer_letter = false
  var clamp_salience = false
  var name = ""

  def workspaceObjectRep(): WorkspaceObjectRep = WorkspaceObjectRep(
    uuid,descriptionReps(),
    letterOrGroupCompanionReps(),
    spans_string,
    group.map(_.groupRep()),
    left_bond.map(_.bondRep()),
    right_bond.map(_.bondRep())
  )


  def relatedGroups(): List[Group] = {
    relatedGroups(List.empty[Group])
  }
  def relatedGroups(acc: List[Group]): List[Group] = {
    group match {
      case Some(g) =>
        g.relatedGroups(g :: acc)
      case None => acc
    }
  }



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


  def add_description(descriptionType: SlipNodeRep, descriptor: Option[SlipNodeRep]) = {
    val description = new Description(this, wString, descriptionType, descriptor)
    descriptions += description
  }



  // move partially to slipnet
  def get_possible_descriptions(info: DescriptionTypeInstanceLinksToNodeInfo): List[SlipNodeRep] = {
    val firstTos = if (has_slipnode_description(info.firstTos.slipNodeRef)) info.firstTos.tos else List.empty[SlipNodeRep]
    val lastTos = if (has_slipnode_description(info.lastTos.slipNodeRef)) info.lastTos.tos else List.empty[SlipNodeRep]
    val numbersTos =
      (0 to 4).toList.map(
        y => if (this.isInstanceOf[Group] && this.asInstanceOf[Group].object_list.size == y+1) info.numbersTos(y).tos else List.empty[SlipNodeRep]
      ).flatten
    val middleTos = if (middle_object()) info.middleTos else List.empty[SlipNodeRep]
    firstTos ::: lastTos ::: numbersTos ::: middleTos
  }


  def has_slipnode_description(ds: SlipNodeRep): Boolean = {
    descriptions.find(d =>
        d.descriptor.isDefined &&
        d.descriptor.get.equals(ds.id)
    ).isDefined
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

  def update_object_value() = {
    // calculate the raw importance of the object
    // = sum of all relevant descriptions
    val rawSum = descriptions.filter(d => d.descriptor.isDefined).map(d => {
      val descriptorActivation = d.descriptor.get.activation
      if (d.descriptionType.activation==100.0) {
        descriptorActivation
      } else {
        descriptorActivation / 20.0
      }
    }).sum

    val groupAdaptedSum = if (group.isDefined) rawSum * 2.0 / 3.0 else rawSum
    val sum = if (changed) groupAdaptedSum * 2.0 else groupAdaptedSum
    raw_importance = sum;

    // calculate the intra-string-happiness of the object
    val result = if (spans_string) {
      100.0
    } else {
      if (group.isDefined) {
        group.get.total_strength
      } else {
        val bondstrengthRaw = bonds.map(b => b.total_strength).sum
        if (spans_string) bondstrengthRaw / 3.0 else bondstrengthRaw / 6.0
      }
    }
    intra_string_happiness = result;


    // calculate intra-string-unhappiness
    intra_string_unhappiness = 100.0-intra_string_happiness;

    // calculate inter-string-happiness
    inter_string_happiness = 0.0;
    if (correspondence.isDefined) inter_string_happiness = correspondence.get.total_strength

    // calculate inter-string-unhappiness
    inter_string_unhappiness = 100.0-inter_string_happiness;

    // calculate total-happienss
    total_happiness = (inter_string_happiness+intra_string_happiness)/2.0;

    // calculate total-unhappiness
    total_unhappiness = 100.0-total_happiness;

    // calculate intra_string_salience
    if (clamp_salience) intra_string_salience = 100.0;
    else intra_string_salience = Formulas.weighted_average(
      relative_importance,0.2,intra_string_unhappiness,0.8);

    // calculate inter_string_salience
    if (clamp_salience) inter_string_salience = 100.0;
    else inter_string_salience = Formulas.weighted_average(
      relative_importance,0.8,inter_string_unhappiness,0.2);

    // calculate total salience
    total_salience = (intra_string_salience+inter_string_salience)/2;
    //System.out.println(this+" salience:"+total_salience+" raw importance:"+raw_importance+" relative importance:"+relative_importance+" intra string salience:"+intra_string_salience+" inter string salience:"+inter_string_salience+" intra su:"+intra_st

    //ring_unhappiness+" inter su:"+inter_string_unhappiness+"
    //total_unhappiness:"+total_unhappiness );

  }

  def get_description(description_type: SlipNodeRep): Option[SlipNodeRep] = {
    // returns the description attached to this object of the specified description type
    descriptions.find(d => d.descriptionType == description_type).map(_.descriptor).flatten
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
  def letterOrGroupCompanionReps(): List[WorkspaceObjectRep] = letterOrGroupCompanions().map(_.workspaceObjectRep())

  def addBond(b: Bond): Unit = {
    bonds += b
  }
  def break_bond(b: Bond): Unit = {
    bonds -= b
  }
  def break_description(d: Description): Unit = {
    descriptions -= d
  }
}

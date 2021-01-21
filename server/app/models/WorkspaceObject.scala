package models

import akka.actor.ActorRef
import akka.protobuf.DescriptorProtos.DescriptorProto
import models.Bond.BondRep
import models.Description.DescriptionRep
import models.Group.GroupRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.DescriptionTypeInstanceLinksToNodeInfo
import models.WorkspaceObject.{WorkspaceObjectRep, WorkspaceObjectRep2}

import scala.collection.mutable.ListBuffer

object WorkspaceObject {
  case class WorkspaceObjectRep(
                                 uuid: String,
                                 descriptions: List[DescriptionRep],
                                 letterOrGroupCompanionReps: List[WorkspaceObjectRep2],
                                 spans_string: Boolean,
                                 groupRep: Option[GroupRep],
                                 asGroupRep: Option[GroupRep],
                                 left_bond: Option[BondRep],
                                 right_bond: Option[BondRep]
                               )


  case class WorkspaceObjectRep2(
                                 uuid: String,
                                 descriptions: List[DescriptionRep],
                                 //letterOrGroupCompanionReps: List[WorkspaceObjectRep],
                                 spans_string: Boolean,
                                 groupRep: Option[GroupRep],
                                 left_bond: Option[BondRep],
                                 right_bond: Option[BondRep]
                               )

}


abstract class WorkspaceObject(ws: WorkspaceString) extends WorkspaceStructure {

  wString = Some(ws)
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
  var bond_descriptions =  ListBuffer.empty[Description] // if it is a group
  // these are the types of bonds holding it together

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
    uuid,
    descriptionReps(),
    letterOrGroupCompanionReps(),
    spans_string,
    group.map(_.groupRep()),
    asGroupRep(),
    left_bond.map(_.bondRep()),
    right_bond.map(_.bondRep())
  )
  def workspaceObjectRep2(): WorkspaceObjectRep2 = WorkspaceObjectRep2(
    uuid,
    descriptionReps(),
    //letterOrGroupCompanionReps(),
    spans_string,
    group.map(_.groupRep()),
    left_bond.map(_.bondRep()),
    right_bond.map(_.bondRep())
  )

  def asGroupRep(): Option[GroupRep] = {
    if (this.isInstanceOf[Group]) {
      Some(this.asInstanceOf[Group].groupRep())
    } else None
  }

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
    var leftmost_neighbor = ws.objects.find(ob => ob.leftmost && (ob.right_string_position == left_string_position-1))
    var rightmost_neighbor = ws.objects.find(ob => ob.rightmost && (ob.left_string_position == right_string_position-1))

    leftmost_neighbor.isDefined && rightmost_neighbor.isDefined
  }


  def letter_span() = right_string_position - left_string_position + 1


  def relevant_descriptions(activationBySlipNodeID: Map[String, Double]): List[Description] = {
    descriptions.toList.filter(d => Workspace.activationWithSlipNodeRep(activationBySlipNodeID, d.description_type) == 100.0)
  }


  override def toString(): String = {
    if (left_string_position==right_string_position)
      s"letter ($left_string_position) $right_string_position"
    else
      s"group ($left_string_position-$right_string_position)"
  }

  def descriptionReps(): List[DescriptionRep] = descriptions.toList.map(d => d.descriptionRep())


  def add_description(descriptionType: SlipNodeRep, descriptor: Option[SlipNodeRep]) = {
    val description = new Description(this, ws, descriptionType, descriptor)
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
    has_slipnode_description_with_id(ds.id)
  }
  def has_slipnode_description_with_id(ds_id: String): Boolean = {
    descriptions.find(d =>
      d.descriptor.isDefined &&
        d.descriptor.get.equals(ds_id)
    ).isDefined
  }


  def has_description(description: Description): Boolean = {
    descriptions.find(d =>
      description.description_type.id.equals(d.description_type.id) &&
        description.descriptor.isDefined &&
        d.descriptor.isDefined &&
        description.descriptor.get.equals(d.descriptor.get.id)
    ).isDefined
  }

  // What is not seen is that in found a description same dt, it can still return None (because descriptor is an option)
  def get_descriptor(dt: SlipNodeRep): Option[SlipNodeRep] = {
    val descriptionWithDescriptionType = descriptions.find(d => d.description_type.id == dt.id)
    descriptionWithDescriptionType match {
      case Some(d) => d.descriptor
      case None => None
    }
  }

  def relevant_distinguishing_descriptors(activationBySlipNodeID: Map[String, Double]) = {
    relevant_descriptions(activationBySlipNodeID).map(d => {
      if (d.descriptor.isDefined && distinguishing_descriptor(d.descriptor.get.id)) {
        Some(d.descriptor.get)
      } else None
    }).flatten
  }


  def distinguishing_descriptor(descriptor_id: String): Boolean = {
    // returns true if no other object of the same type (ie. letter or group)
    // has the same descriptor

    if (descriptor_id == SlipNode.id.letter ||
      descriptor_id == SlipNode.id.group ||
      Slipnet.isNumber(descriptor_id)
    ) {
      false
    } else {

      ws.objects.find(wo => {
        if (wo == this) {
          true
        } else {
          // check to see if they are of the same type
          if (
              (this.isInstanceOf[Letter] && wo.isInstanceOf[Letter]) ||
              (this.isInstanceOf[Group] && wo.isInstanceOf[Group])
          ) {
            // check all descriptions for the descriptor
            wo.descriptions.find(d => d.descriptor.isDefined && d.descriptor.get.id == descriptor_id).isEmpty

          } else true
        }

      }).isDefined
    }
  }

  def recursive_group_member(group: WorkspaceObject): Boolean = {
    if ((left_string_position>=group.left_string_position)&&
      (right_string_position<=group.right_string_position))
      return true;
    return false;
  }

  def update_object_value(activationBySlipNodeID: Map[String, Double]) = {
    // calculate the raw importance of the object
    // = sum of all relevant descriptions
    val rawSum = descriptions.filter(d => d.descriptor.isDefined).map(d => {
      val descriptorActivation = Workspace.activationWithSlipNodeRep(activationBySlipNodeID, d.descriptor.get)
      val description_typeActivation = Workspace.activationWithSlipNodeRep(activationBySlipNodeID, d.description_type)

      System.out.println("update_object_value " + d + " d.description_type.activation " + description_typeActivation + " d.descriptor.activation " + descriptorActivation);

      if (description_typeActivation == 100.0) {
        descriptorActivation
      } else {
        descriptorActivation / 20.0
      }
    }).sum
    System.out.println("update_object_value " + rawSum + " group.isDefined " + group.isDefined + " changed " +changed);

    val groupAdaptedSum = if (group.isDefined) rawSum * 2.0 / 3.0 else rawSum
    val sum = if (changed) groupAdaptedSum * 2.0 else groupAdaptedSum
    raw_importance = sum;
    System.out.println("update_object_value : raw_importance " + raw_importance);

    // calculate the intra-string-happiness of the object
    val result = if (spans_string) {
      100.0
    } else {
      if (group.isDefined) {
        group.get.total_strength
      } else {
        val bondstrengthRaw = bonds.map(b => b.total_strength).sum
        System.out.println("update_object_value : bondstrengthRaw " + bondstrengthRaw);

        if (spans_string) bondstrengthRaw / 3.0 else bondstrengthRaw / 6.0
      }
    }
    System.out.println("update_object_value : intra_string_happiness " + result);

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

  def letter_distance(ob2: WorkspaceObject) = {
    if (ob2.left_string_position > right_string_position) {
       ob2.left_string_position - right_string_position
    } else if (left_string_position > ob2.right_string_position) {
      left_string_position-ob2.right_string_position
    } else 0
  }



  def get_description(description_type: SlipNodeRep): Option[SlipNodeRep] = {
    // returns the description attached to this object of the specified description type
    get_description_with_id(description_type.id)
  }
  def get_description_with_id(description_type: String): Option[SlipNodeRep] = {
    descriptions.find(d => d.description_type.id == description_type).map(_.descriptor).flatten
  }

  def get_description_type(description: Option[SlipNodeRep]): Option[SlipNodeRep] = {
    // returns the description_type attached to this object of the specified description
    descriptions.find(d => d.descriptor == description).map(_.description_type)
  }

  def letterOrGroupCompanions(): List[WorkspaceObject] = {
      ws.objects.filter(wo =>
        (wo != this) &&
          (isInstanceOf[Letter] && wo.isInstanceOf[Letter]) ||
          (isInstanceOf[Group] && wo.isInstanceOf[Group])
      ).toList
  }
  def letterOrGroupCompanionReps(): List[WorkspaceObjectRep2] = letterOrGroupCompanions().map(_.workspaceObjectRep2())

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

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
import models.Workspace.{CommonSubProcessing, DataForStrengthUpdateResponse, GoWithBottomUpCorrespondenceScout2, InitializeWorkspaceStringsResponse, PostTopBottomCodeletsGetInfoResponse, SlipnetLookAHeadForNewBondCreationResponse, SlippageListShell, UpdateEverythingFollowUp}
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport
import Description.DescriptionRep
import models.Bond.BondRep
import models.Coderack.{Post, SlipnetUpdateEverythingResponse}
import models.ConceptMapping.{ConceptMappingParameters, ConceptMappingRep, ConceptMappingRep2}
import models.Correspondence.CorrespondenceRep
import models.ExecutionRun.InitializeSlipnetResponse
import models.Group.{FutureGroupRep, GroupRep}
import models.Letter.LetterSlipnetComplement
import models.SlipNode.{GroupSlipnetInfo, SlipNodeRep, SlipnetInfo}
import models.WorkspaceObject.WorkspaceObjectRep
import models.WorkspaceStructure.WorkspaceStructureRep
import models.codelet.BottomUpDescriptionScout.SlipnetGoWithBottomUpDescriptionScoutResponse
import models.codelet.{CodeletType, CodeletTypeString}
import models.codelet.CorrespondenceBuilder.{SlipnetGoWithCorrespondenceBuilder4Response, SlipnetGoWithCorrespondenceBuilder5Response, SlipnetGoWithCorrespondenceBuilderResponse, SlipnetGoWithCorrespondenceBuilderResponse2, SlipnetGoWithCorrespondenceBuilderResponse3}
import models.codelet.CorrespondenceStrengthTester.{SlipnetGoWithCorrespondenceStrengthTester2Response, SlipnetGoWithCorrespondenceStrengthTesterResponse}
import models.codelet.GroupBuilder.{PrepareBondFightingResponse, SlipnetGoWithGroupBuilderResponse, SlipnetPrepareGroupFightingResponse}
import models.codelet.GroupScoutWholeString.{GetLeftAndRightResponse, SlipnetGoWithGroupScoutWholeStringResponse}
import models.codelet.GroupStrengthTester.{SlipnetGoWithGroupStrengthTesterResponse, SlipnetGoWithGroupStrengthTesterResponse2}
import models.codelet.ImportantObjectCorrespondenceScout.SlipnetGoWithImportantObjectCorrespondenceScoutResponse
import models.codelet.RuleScout.{RuleScoutProposeRule, SlipnetGoWithRuleScoutResponse}
import models.codelet.TopDownBondScoutCategory.SlipnetTopDownBondScoutCategory2Response
import models.codelet.TopDownBondScoutDirection.SlipnetTopDownBondScoutDirection2Response
import models.codelet.TopDownDescriptionScout.{SlipnetGoWithTopDownDescriptionScoutResponse, SlipnetGoWithTopDownDescriptionScoutResponse2}
import models.codelet.TopDownGroupScoutCategory.{SlipnetGoWithTopDownGroupScoutCategory2Response, SlipnetGoWithTopDownGroupScoutCategoryResponse}
import models.codelet.TopDownGroupScoutDirection.SlipnetGoWithTopDownGroupScoutDirection0Response
//import models.codelet.TopDownGroupScoutDirection.SlipnetGoWithTopDownGroupScoutDirectionResponse

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object Slipnet {
  def isNumber(descriptor_id: String): Boolean = {
    slipnet_numbers.find(sn => sn.id == descriptor_id).isDefined
  }

  var slipnet_numbers: List[SlipNodeRep] = null



  def props(workspace: ActorRef): Props = Props(new Slipnet(workspace))

  /*object DirValue extends Enumeration {
    type DirValue = Value
    val Left, Right, None = Value
  }*/


  case class Run(initialString: String, modifiedString: String, targetString: String)
  case class InitializeSlipnet(coderack: ActorRef, workspace: ActorRef)
  case class UpdateEverything(t:Double)
  case class InitializeWorkspaceStrings(initialWos: List[LetterSlipnetComplement],
                                        modifiedWos: List[LetterSlipnetComplement],
                                        targetWos: List[LetterSlipnetComplement]
                                       )

  case class BondFromTo(from: WorkspaceObjectRep, to: WorkspaceObjectRep)
  case class SlipnetTopDownBondScout(fromdtypes: List[SlipNodeRep], todtypes: List[SlipNodeRep])
  case class SlipnetTopDownBondScoutCategory2(bondCategory: String, from_descriptor: Option[SlipNodeRep], to_descriptor: Option[SlipNodeRep], bondFacet: SlipNodeRep)
  case class SlipnetTopDownBondScoutDirection2(bondCategory: String, from_descriptor: Option[SlipNodeRep], to_descriptor: Option[SlipNodeRep], bondFacet: SlipNodeRep)

  case class BondFromTo2(
                          from: WorkspaceObjectRep,
                          to: WorkspaceObjectRep,
                          fromDescriptor: Option[SlipNodeRep],
                          toDescriptor: Option[SlipNodeRep],
                          bondFacet: SlipNodeRep
                        )

  case class SlipnetBottomUpCorrespondenceScout(
                                       obj1 :WorkspaceObjectRep,
                                       obj2 :WorkspaceObjectRep,
                                       temperature: Double
                                     )
  case class SlipnetBottomUpCorrespondenceScout2(obj1: WorkspaceObjectRep, obj2: WorkspaceObjectRep)
  /*case class ProposeAnyCorrespondence2(
                                       obj1 :WorkspaceStructureRep,
                                       obj2: WorkspaceStructureRep,
                                       //temperature: Double,
                                       codelet: ActorRef
                                      )*/
  case class CompleteProposeGroup(grCategory: SlipNodeRep, dirCategoryID: Option[SlipNodeRep])
  case class CompleteProposeGroupResponse(urgency: Double, bond_category: SlipNodeRep, group_category: SlipNodeRep)

  case class SlipnetGoWithTopDownDescriptionScout(chosen_object: WorkspaceObjectRep, descriptionTypeID: String)
  case class SlipnetGoWithTopDownDescriptionScout2(chosen_property: SlipNodeRep)


  case class GoWithTopDownBondScout2Response(bond_facet: SlipNodeRep, from_descriptor: Option[SlipNodeRep], to_descriptor: Option[SlipNodeRep])

  case class SetSlipNodeBufferValue(slipNodeID: String, bufferValue: Double)
  case class SlipnetGoWithBottomUpDescriptionScout(slipNodeRep: SlipNodeRep, temperature: Double)

  case class SlipnetTopDownBondScoutResponse(bondfacets: List[SlipNodeRep])

  case class TosInfo(slipNodeRef: SlipNodeRep, tos: List[SlipNodeRep])
  case class DescriptionTypeInstanceLinksToNodeInfo(
                                                     firstTos: TosInfo,
                                                     lastTos: TosInfo,
                                                     numbersTos: Map[Int, TosInfo],
                                                     middleTos: List[SlipNodeRep]
                                                   )
  case class SlipnetGoWithTopDownGroupScoutCategory(groupID: String, temperature: Double)
  case class SlipnetGoWithTopDownGroupScoutCategory2(group_cat_id:String, dir: Option[SlipNodeRep])
  case class SlipnetGoWithTopDownGroupScoutCategory3(bond_category: SlipNodeRep, fromOBRep: WorkspaceObjectRep)
  case object SlipnetGoWithTopDownGroupScoutDirection0
  case class SlipnetGoWithTopDownGroupScoutDirection(bond_category: SlipNodeRep, first_bond: String, fromob: String)

  case class SlipnetGoWithGroupScoutWholeString(bc: SlipNodeRep)
  case class SlipnetGoWithGroupBuilder(group_category_id: String)
  case class SlipnetPrepareGroupFighting(group_category_id: String, incg: List[String], group_category_id_by_group_id: Map[String, String])
  case class InflatedDescriptionRep(
                                     uuid :String,
                                     descriptionTypeSlipNode: SlipNode,
                                     descriptorSlipNode: Option[SlipNode]) {
    //def descriptionType(mapping: Map[String, SlipNode]) = mapping()
  }

  case class GetRelatedNodeOf(slipnodeID: String, lookingAtSlipNodeID: String)
  case class GetRelatedNodeOfResponse(related: Option[SlipNodeRep])
  case object GetLeftAndRight
  case class SlipnetGoWithGroupStrengthTester(group_category_id: String)
  case class SlipnetGoWithGroupStrengthTester2(g: GroupRep, strength: Double)
  case class SlipnetLookAHeadForNewBondCreation(s: ActorRef, group: GroupRep, index: Int, incg: List[String], newBondList: List[BondRep],                                                          from_obj_id: String,
                                                to_obj_id: String
                                               )
  case class SlipnetProposeRule(description: Option[SlipNodeRep], relation: Option[SlipNodeRep])
  case class SlipnetProposeRuleResponse(
                                         urgency: Double,
                                         lengthSlipNode: SlipNodeRep,
                                         predecessorSlipNode: SlipNodeRep,
                                         successorSlipNode: SlipNodeRep
                                       )
  case object SlipnetGoWithRuleScout
  case class SlipnetCompleteSlippageList(slippagesShell: SlippageListShell)
  case class SlipnetCompleteSlippageListResponse(slippage_list_rep: List[ConceptMappingRep])

  case class SlipnetGoWithRuleScout3(
                                      object_list: List[SlipNodeRep],
                                      changedReplacementRelation: Option[String],
                                      letterCategory: SlipNodeRep,
                                      temperature: Double)

  case class SlipnetGoWithImportantObjectCorrespondenceScout(relevantDescriptors: List[SlipNodeRep], t: Double)
  case class SlipnetGoWithImportantObjectCorrespondenceScout2(obj1: WorkspaceObjectRep,  obj2: WorkspaceObjectRep, obj2GroupRep: WorkspaceObjectRep, t: Double)
  case class GroupFlippedVersion(obj: GroupRep)
  case class GroupFlippedVersionResponse(fgr: Option[FutureGroupRep])
  case class SlipnetGoWithCorrespondenceBuilder(conceptMappingReps: List[ConceptMappingRep])
  case class SlipnetGoWithCorrespondenceBuilder2(
                                                  correspondence: CorrespondenceRep,
                                                  correspondenceReps: List[CorrespondenceRep],
                                                  wCReps: List[CorrespondenceRep]
                                                )
  case class SlipnetGoWithCorrespondenceBuilder3(correspondence: CorrespondenceRep, incompatible_bond_base: Option[(BondRep,BondRep)], workspaceCorrespondences: List[CorrespondenceRep])
  case class SlipnetGoWithCorrespondenceBuilder4(c: CorrespondenceRep)
  case class CheckOnBothObjectsSpanningForStringDescriptionFlip(
                                                                 conceptMappingReps: List[ConceptMappingRep],
                                                                 distinguishingMappingReps: List[ConceptMappingRep],
                                                                 obj1: WorkspaceObjectRep,
                                                                 obj2: WorkspaceObjectRep
                                                               )
  case class ProposeAnyCorrespondenceSlipnetResponse(fg: FutureGroupRep)
  case class ProposeAnyCorrespondenceSlipnetResponse2(
                                                       obj1: WorkspaceObjectRep,
                                                       obj2: WorkspaceObjectRep,
                                                       concept_mapping_list : List[ConceptMappingRep],
                                                       flip_obj2: Boolean,
                                                       distiguishingConceptMappingSize: Int,
                                                       distiguishingConceptMappingTotalStrength: Double
                                                     )
  case class SlipnetGoWithCorrespondenceBuilder5(groupObjs: Option[ConceptMappingParameters])
  case class SlipnetGoWithCorrespondenceBuilder6(cms: List[ConceptMappingRep])
  case class SlipnetGoWithBondStrengthTester(bondRep: BondRep)
  case class SlipnetGoWithCorrespondenceStrengthTester(c: CorrespondenceRep, workspaceCorrespondences: List[CorrespondenceRep])
  case class SlipnetGoWithCorrespondenceStrengthTester2(c: CorrespondenceRep)

  case class DataForStrengthUpdate(brs: List[BondRep], crs: List[CorrespondenceRep], t: Double)
  case class CorrespondenceUpdateStrengthData(internal_strength: Double, supporting_correspondences:Map[String, Boolean])
  case class PostTopBottomCodeletsGetInfo(
                                           t: Double,
                                           intra_string_unhappiness: Double,
                                           inter_string_unhappiness: Double,
                                           ruleTotalWeaknessOpt: Option[Double],
                                           number_of_bonds: Int,
                                           unrelated_objects_size: Int,
                                           ungrouped_objects_size: Int,
                                           unreplaced_objects_size: Int,
                                           uncorresponding_objects_size: Int,
                                             codeletsSize:Int
  )
  case class SlipnetGoWithBondStrengthTesterResponse(bond_category_degree_of_association: Double)
  case class PrepareBondFighting(groupID: String, bondReps: List[BondRep])


  /*object RelationType {
    val Sameness = "Sameness"
    val Successor = "Successor"
    val Predecessor = "Predecessor"
  }*/
  val time_step_length = 15


  def choose_slipnode_by_conceptual_depth(slist: List[SlipNode], t: Double): Option[SlipNode] = {
    if (slist.size == 0) {
      None
    } else {
      val object_probs = slist.map(s => {
        Formulas.temperature_adjusted_probability(s.conceptual_depth, t)
      })
      val index = Utilities.valueProportionalRandomIndexInValueList(object_probs)
      Some(slist(index))
    }
  }

}


class Slipnet(workspace: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {

  import Coderack.{ Run, ProposeCorrespondence}
  import Slipnet._
  import models.codelet.Codelet.Finished
  import models.codelet.BottomUpBondScout.{ BondFromToSlipnetResponse, BondFromTo2Response }

  var woAppActor: Option[ActorRef] = None
  var coderack: ActorRef = null


  var remove_spreading_activation = false
  var remove_activation_jump = false

  var slipNodeRefs = Map.empty[String, SlipNode]
  var slipNodes = ListBuffer.empty[SlipNode]

  //var sameness = Option.empty[SlipNode]
  //var opposite = Option.empty[SlipNode]
  //var group_category = Option.empty[SlipNode]
  //var direction_category = Option.empty[SlipNode]

  var chars = (0 to 25).map(i => (i + 65).toChar.toString)
  var slipnet_letters = chars.map(c => addBasicSlipNode(0, 0, 10.0, c, c)).to[ListBuffer]

  var numbers = (0 to 4).map(i => (i + 49).toChar.toString)
  val slipnet_numbers: ListBuffer[SlipNode] = numbers.map(c => addBasicSlipNode(0, 0, 30.0, c, c)).to[ListBuffer]
  Slipnet.slipnet_numbers = slipnet_numbers.toList.map(_.slipNodeRep())



  // moved to singleton
  // val time_step_length = 15
  var number_of_updates = 0

  // string positions
  val leftmost = add_slipnode(17, 18, 40.0, "leftmost", SlipNode.id.leftmost)
  val rightmost = add_slipnode(27, 18, 40.0, "rightmost", SlipNode.id.rightmost)
  val middle = add_slipnode(27, 26, 40.0, "middle", SlipNode.id.middle)
  val single = add_slipnode(33, 26, 40.0, "single", "sl")
  val whole = add_slipnode(30, 26, 40.0, "whole", "wh")

  // alphabetic positions
  val first = add_slipnode(19, 15, 60.0, "first", "fs")
  val last = add_slipnode(25, 15, 60.0, "last", "ls")

  // directions
  val left = add_slipnode(17, 22, 40.0, "left", SlipNode.id.left)
  left.codelets += CodeletTypeString.TopDownBondScoutDirection
  left.codelets += CodeletTypeString.TopDownGroupScoutDirection
  val right = add_slipnode(27, 22, 40.0, "right", SlipNode.id.right)
  right.codelets += CodeletTypeString.TopDownBondScoutDirection
  right.codelets += CodeletTypeString.TopDownGroupScoutDirection

  // bond types
  val predecessor = add_slipnode_with_len(14, 38, 50.0, "predecessor", SlipNode.id.predecessor  ,60.0)
  predecessor.codelets += CodeletTypeString.TopDownBondScoutCategory

  val successor = add_slipnode_with_len(14, 33, 50.0, "successor", SlipNode.id.successor , 60.0)
  successor.codelets += CodeletTypeString.TopDownBondScoutCategory
  val sameness = add_slipnode_with_len(10, 29, 80.0, "sameness", SlipNode.id.sameness,0.0)
  sameness.codelets += CodeletTypeString.TopDownBondScoutCategory

  // group types
  val predgrp = add_slipnode(20, 38, 50.0, "predecessor group", "pg")
  predgrp.codelets += CodeletTypeString.TopDownGroupScoutCategory
  val succgrp = add_slipnode(20, 33, 50.0, "successor group", SlipNode.id.successor_group)
  succgrp.codelets += CodeletTypeString.TopDownGroupScoutCategory
  val samegrp = add_slipnode(10, 25, 80.0, "sameness group", "smg")
  samegrp.codelets += CodeletTypeString.TopDownGroupScoutCategory

  // other relations
  val identity = add_slipnode_with_len(2, 30, 90.0, "identity", "id", 0.0)
  val opposite = add_slipnode_with_len(6, 30, 90.0, "opposite", "op", 80.0)

  // objects
  val letter = add_slipnode(2, 38, 20.0, "letter", "l")
  val group = add_slipnode(6, 38, 80.0, "group", "g")

  // categories
  val letter_category = add_slipnode(22, 9, 30.0, "letter category", SlipNode.id.letter_category)
  val string_position_category = add_slipnode(30, 21, 70.0, "string position", "spc")
  string_position_category.codelets += CodeletTypeString.TopDownDescriptionScout
  val alphabetic_position_category = add_slipnode(22, 12, 80.0, "alphabetic position", "apc")
  alphabetic_position_category.codelets += CodeletTypeString.TopDownDescriptionScout
  val direction_category = add_slipnode(22, 25, 70.0, "direction category", "dc")
  val bond_category = add_slipnode(10, 33, 80.0, "bond category", SlipNode.id.bond_category)
  val group_category = add_slipnode(17, 29, 80.0, "group category", "gpc")
  val length = add_slipnode(36, 32, 60.0, "length", "len")
  val object_category = add_slipnode(4, 34, 90.0, "object category", "obc")
  val bond_facet = add_slipnode(36, 26, 90.0, "bond facet", "bf")

  // specify the descriptor types that bonds can form between
  var bond_facets = List(letter_category).to[ListBuffer]
  bond_facets += length

  // add initially_clamped_slipnodes
  var initially_clamped_slipnodes = List(letter_category).to[ListBuffer]
  letter_category.clamp = true;
  initially_clamped_slipnodes += string_position_category
  string_position_category.clamp = true;
  var remove_breaker_codelets = false;
  var speed_up_bonds = false;

  //***************************************************************
  //   initialise links between nodes

  // **************   successor and predecessor links
  var sliplinks = ListBuffer.empty[SlipnetLink]
  // letters
  linkSuccessiveSlipNodes(slipnet_letters)
  linkSuccessiveSlipNodes(slipnet_numbers)
  // ************** letter category links
  linkWith(slipnet_letters, letter_category, 97.0)
  add_category_link(samegrp, letter_category, 50.0);

  // *************** length links
  linkWith(slipnet_numbers, length, 100.0)

  add_nonslip_link(predgrp, length, 95.0);
  add_nonslip_link(succgrp, length, 95.0);
  add_nonslip_link(samegrp, length, 95.0);

  // *************** opposite links
  add_slip_link(first, last, opposite);
  add_slip_link(last, first, opposite);
  add_slip_link(leftmost, rightmost, opposite);
  add_slip_link(rightmost, leftmost, opposite);
  add_slip_link(left, right, opposite);
  add_slip_link(right, left, opposite);
  add_slip_link(successor, predecessor, opposite);
  add_slip_link(predecessor, successor, opposite);
  add_slip_link(succgrp, predgrp, opposite);
  add_slip_link(predgrp, succgrp, opposite);

  // ***************** has property links
  add_property_link(slipnet_letters(0), first, 75.0)
  add_property_link(slipnet_letters(25), last, 75.0)

  // ******************* object category links
  add_category_link(letter, object_category, object_category.conceptual_depth - letter.conceptual_depth)
  add_instance_link(object_category, letter, 100.0)
  add_category_link(group, object_category, object_category.conceptual_depth - group.conceptual_depth)
  add_instance_link(object_category, group, 100.0)

  // string position links
  add_category_link(leftmost, string_position_category, string_position_category.conceptual_depth - leftmost.conceptual_depth)
  add_instance_link(string_position_category, leftmost, 100.0)
  add_category_link(rightmost, string_position_category, string_position_category.conceptual_depth - rightmost.conceptual_depth)
  add_instance_link(string_position_category, rightmost, 100.0)
  add_category_link(middle, string_position_category, string_position_category.conceptual_depth - middle.conceptual_depth);
  add_instance_link(string_position_category, middle, 100.0);
  add_category_link(single, string_position_category, string_position_category.conceptual_depth - single.conceptual_depth);
  add_instance_link(string_position_category, single, 100.0);
  add_category_link(whole, string_position_category, string_position_category.conceptual_depth - whole.conceptual_depth);
  add_instance_link(string_position_category, whole, 100.0);

  // alphabetic position category
  add_category_link(first, alphabetic_position_category, alphabetic_position_category.conceptual_depth - first.conceptual_depth);
  add_instance_link(alphabetic_position_category, first, 100.0);
  add_category_link(last, alphabetic_position_category, alphabetic_position_category.conceptual_depth - last.conceptual_depth);
  add_instance_link(alphabetic_position_category, last, 100.0);

  // direction-category links
  add_category_link(left, direction_category, direction_category.conceptual_depth - left.conceptual_depth);
  add_instance_link(direction_category, left, 100.0);
  add_category_link(right, direction_category, direction_category.conceptual_depth - right.conceptual_depth);
  add_instance_link(direction_category, right, 100.0);

  // bond-category links
  add_category_link(predecessor, bond_category, bond_category.conceptual_depth - predecessor.conceptual_depth);
  add_instance_link(bond_category, predecessor, 100.0);
  add_category_link(successor, bond_category, bond_category.conceptual_depth - successor.conceptual_depth);
  add_instance_link(bond_category, successor, 100.0);
  add_category_link(sameness, bond_category, bond_category.conceptual_depth - sameness.conceptual_depth);
  add_instance_link(bond_category, sameness, 100.0);

  // group-category links
  add_category_link(predgrp, group_category, group_category.conceptual_depth - predgrp.conceptual_depth);
  add_instance_link(group_category, predgrp, 100.0);
  add_category_link(succgrp, group_category, group_category.conceptual_depth - succgrp.conceptual_depth);
  add_instance_link(group_category, succgrp, 100.0);
  add_category_link(samegrp, group_category, group_category.conceptual_depth - samegrp.conceptual_depth);
  add_instance_link(group_category, samegrp, 100.0);

  // associated-group links
  add_nonslip_link(sameness, samegrp, group_category).fixed_length = 30.0
  add_nonslip_link(successor, succgrp, group_category).fixed_length = 60.0
  add_nonslip_link(predecessor, predgrp, group_category).fixed_length = 60.0

  // associated bond links
  add_nonslip_link(samegrp, sameness, bond_category).fixed_length = 90.0
  add_nonslip_link(succgrp, successor, bond_category).fixed_length = 90.0
  add_nonslip_link(predgrp, predecessor, bond_category).fixed_length = 90.0

  // bond facet links
  add_category_link(letter_category, bond_facet, bond_facet.conceptual_depth - letter_category.conceptual_depth);
  add_instance_link(bond_facet, letter_category, 100.0);
  add_category_link(length, bond_facet, bond_facet.conceptual_depth - length.conceptual_depth);
  add_instance_link(bond_facet, length, 100.0);

  // letter category links
  add_slip_link(letter_category, length, 95.0);
  add_slip_link(length, letter_category, 95.0);

  // letter group links
  add_slip_link(letter, group, 90.0);
  add_slip_link(group, letter, 90.0);

  // direction-position, direction-neighbor, position-neghbor links
  add_nonslip_link(left, leftmost, 90.0);
  add_nonslip_link(leftmost, left, 90.0);
  add_nonslip_link(right, leftmost, 100.0);
  add_nonslip_link(leftmost, right, 100.0);
  add_nonslip_link(right, rightmost, 90.0);
  add_nonslip_link(rightmost, right, 90.0);
  add_nonslip_link(left, rightmost, 100.0);
  add_nonslip_link(rightmost, left, 100.0);
  add_nonslip_link(leftmost, first, 100.0);
  add_nonslip_link(first, leftmost, 100.0);
  add_nonslip_link(rightmost, first, 100.0);
  add_nonslip_link(first, rightmost, 100.0);
  add_nonslip_link(leftmost, last, 100.0);
  add_nonslip_link(last, leftmost, 100.0);
  add_nonslip_link(rightmost, last, 100.0);
  add_nonslip_link(last, rightmost, 100.0);

  // other links
  add_slip_link(single, whole, 90.0);
  add_slip_link(whole, single, 90.0);


  // help methods
  def groupSlipnetInfo() = GroupSlipnetInfo(
    bond_facet.slipNodeRep(),
    bond_category.slipNodeRep(),
    object_category.slipNodeRep(),
    group.slipNodeRep(),
    group_category.slipNodeRep(),
    direction_category.slipNodeRep(),
    string_position_category.slipNodeRep(),
    whole.slipNodeRep(),
    leftmost.slipNodeRep(),
    rightmost.slipNodeRep(),
    middle.slipNodeRep(),
    length.slipNodeRep(),
    samegrp.slipNodeRep(),
    letter.slipNodeRep(),
    letter_category.slipNodeRep(),
    right.slipNodeRep(),
    left.slipNodeRep(),
    slipnet_numbers.toList.map(_.slipNodeRep())
  )
  def linkSuccessiveSlipNodes(nodes: ListBuffer[SlipNode]) = for (i <- 0 to nodes.size - 2) {
    add_nonslip_link(nodes(i), nodes(i + 1), successor);
    add_nonslip_link(nodes(i + 1), nodes(i), predecessor);
  }

  def linkWith(nodes: ListBuffer[SlipNode], withSplipNode: SlipNode, len: Double) = {
    for (i <- 0 to nodes.size - 1) {
      add_category_link(nodes(i), withSplipNode, withSplipNode.conceptual_depth - nodes(i).conceptual_depth);
      add_instance_link(withSplipNode, nodes(i), len);
    }
  }


  def add_category_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr, to, lab);
    sliplinks += nl
    fr.category_links += nl
    to.incoming_links += nl
    nl
  }

  def add_instance_link(fr: SlipNode, to: SlipNode, lab: SlipNode): SlipnetLink = {
    val nl = new SlipnetLink(fr, to, lab);
    sliplinks += nl
    fr.instance_links += nl
    to.incoming_links += nl
    nl;
  }

  def add_instance_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr, to, lab);
    sliplinks += nl
    fr.instance_links += nl
    to.incoming_links += nl
    nl;
  }


  def add_property_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr, to, lab);
    sliplinks += nl
    fr.has_property_links += nl
    to.incoming_links += nl
    nl;
  }


  def add_nonslip_link(fr: SlipNode, to: SlipNode, lab: SlipNode): SlipnetLink = {
    val nl = new SlipnetLink(fr, to, lab);
    sliplinks += nl
    fr.lateral_nonslip_links += nl
    to.incoming_links += nl
    nl;
  }

  def add_nonslip_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr, to, lab);
    sliplinks += nl
    fr.lateral_nonslip_links += nl
    to.incoming_links += nl
    nl;
  }


  def add_slip_link(fr: SlipNode, to: SlipNode, lab: SlipNode): SlipnetLink = {
    val nl = new SlipnetLink(fr, to, lab);
    nl.slip_link = true;

    sliplinks += nl
    fr.lateral_slip_links += nl
    to.incoming_links += nl
    nl;
  }

  def add_slip_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr, to, lab);
    nl.slip_link = true;

    sliplinks += nl
    fr.lateral_slip_links += nl
    to.incoming_links += nl
    nl;
  }


  def addBasicSlipNode(x: Int, y: Int, cd: Double, pn: String, sn: String): SlipNode = {
    val slipNode = new SlipNode(x, y, cd, pn, sn, workspace)
    slipNodeRefs += (slipNode.id() -> slipNode)
    slipNodes += slipNode
    slipNode
  }
  def addBasicSlipNodeWithLen(x: Int, y: Int, cd: Double, pn: String,sn: String, len: Double): SlipNode = {
    val slipNode = new SlipNode(x, y, cd, pn,sn, len, workspace)
    slipNodeRefs += (slipNode.id() -> slipNode)
    slipNodes += slipNode
    slipNode
  }

  def add_slipnode(x: Int, y: Int, cd: Double, pn: String, sn: String): SlipNode = {
    addBasicSlipNode(x * 25, y * 25, cd, pn, sn)
  }
  def add_slipnode_with_len(x: Int, y: Int, cd: Double, pn: String,sn: String, len: Double): SlipNode = {
    addBasicSlipNodeWithLen(x * 25, y * 25, cd, pn, sn, len)
  }

  def relevant_descriptions(descriptions: List[InflatedDescriptionRep]) = {
    descriptions.filter(d => d.descriptionTypeSlipNode.activation.equals(100.0))
  }

  def inflatedDescriptionRep(rep: DescriptionRep): InflatedDescriptionRep = {
    val descriptionTypeSlipNode = slipNodeRefs(rep.descriptionType.id)
    val descriptorSlipNode = rep.descriptor match {
      case Some(rep) => Some(slipNodeRefs(rep.id))
      case None => None
    }
    InflatedDescriptionRep(rep.uuid, descriptionTypeSlipNode, descriptorSlipNode)
  }


  def groupFlipped_version(groupRep: GroupRep): Option[FutureGroupRep] = {
    val bond_list = groupRep.bond_list

    // returns a flipped version of this group
    val new_bond_list = bond_list.map(b => bondFlipped_version(b))
    val group_category = slipNodeRefs(groupRep.group_category.id)
    if (groupRep.direction_category.isDefined) {
      val direction_category = slipNodeRefs(groupRep.direction_category.get.id)
      val relatedGroup_category = SlipnetFormulas.get_related_node(group_category, opposite, identity).get
      val relatedDirection_categoryOpt = SlipnetFormulas.get_related_node(direction_category, opposite, identity)
      val relatedBond_category = SlipnetFormulas.get_related_node(group_category, bond_category, identity).get

      val flippedGroup = FutureGroupRep(
        relatedGroup_category.slipNodeRep(),
        relatedDirection_categoryOpt.map(_.slipNodeRep()),
        groupRep.bond_facet,
        relatedBond_category.slipNodeRep(),
        new_bond_list,
        groupSlipnetInfo()
      )

      Some(flippedGroup)

    } else {
      None
    }
  }



  def bondFlipped_version(bond: BondRep): BondRep = {
    val bond_category = slipNodeRefs(bond.bondCategorySlipNodeID)
    val related = SlipnetFormulas.get_related_node(bond_category,opposite, identity).get
    //returns the flipped version of this bond
    BondRep(
      "uuid",
      bond.to_obj,
      bond.from_obj,
      related.id(),
      bond.bondFacetSlipNodeID,
      bond.to_obj_descriptorSlipNodeID, // swapped with below
      bond.from_obj_descriptorSlipNodeID, // swapped with above
      bond.direction_category
    )

  }



  def letterDescriptionReps(wos: List[LetterSlipnetComplement]) = {
    wos.map(l => {
      var descriptions = ListBuffer.empty[DescriptionRep]
      descriptions += DescriptionRep("NotYes", object_category.slipNodeRep(),Some(letter.slipNodeRep()))
      descriptions += DescriptionRep("NotYes",letter_category.slipNodeRep(),Some(slipnet_letters(l.letval).slipNodeRep()))

      val x = l.x
      val len = l.len
      if ((x==0)&&(len==1)) descriptions += DescriptionRep("NotYes",string_position_category.slipNodeRep(),Some(single.slipNodeRep()))
      if ((x==0)&&(len>1)) descriptions += DescriptionRep("NotYes",string_position_category.slipNodeRep(),Some(leftmost.slipNodeRep()))
      if ((x==(len-1))&&(len>1)) descriptions += DescriptionRep("NotYes",string_position_category.slipNodeRep(),Some(rightmost.slipNodeRep()))
      if ((len>2)&&((x*2)==(len-1))) descriptions += DescriptionRep("NotYes",string_position_category.slipNodeRep(),Some(middle.slipNodeRep()))
      WorkspaceObjectRep(l.uuid, descriptions.toList, List(), false, None, None,None, None)
    })


  }


  def receive = LoggingReceive {
    // to the browser

    case Run(initialString, modifiedString, targetString) => {
      log.debug(s"Slipnet | Run with initial $initialString, modified: $modifiedString and target: $targetString")
    }
    case InitializeSlipnet(cr, ws) =>
      log.debug("Slipnet: InitializeSlipnet")
      coderack = cr
      reset()
      set_conceptual_depths(50.0)
      sender() ! InitializeSlipnetResponse


    case UpdateEverything(t) =>
      log.debug("Slipnet. UpdateEverything")
      update()
      workspace ! UpdateEverythingFollowUp

    case InitializeWorkspaceStrings(initialWos, modifiedWos, targetWos) =>
      log.debug("Slipnet: InitializeWorkspaceStrings")
      val initialDescriptions = letterDescriptionReps(initialWos)
      val modifiedDescriptions = letterDescriptionReps(modifiedWos)
      val targetDescriptions = letterDescriptionReps(targetWos)
      workspace ! InitializeWorkspaceStringsResponse(initialDescriptions, modifiedDescriptions, targetDescriptions)


    case SetSlipNodeBufferValue(slipNodeID: String, bufferValue: Double) =>
      slipNodeRefs(slipNodeID).setBuffer(bufferValue)

    // bottom-up-bond-scout codelet.java.255
    case BondFromTo(from, to) =>
      log.debug(s"BondFromTo: Slipnet bond facets $bond_facets")
      // choose_bond_facet, workspace_formulas.java.191
      val fromDescriptionFacets: List[SlipNode] = facetsOfAndPartOf(from, bond_facets.toList)
      val toDescriptionFacets: List[SlipNode] = facetsOfAndPartOf(to, fromDescriptionFacets)

      val fromFacetSlipNodeReps = fromDescriptionFacets.map(_.slipNodeRep())
      val toFacetSlipNodeReps = toDescriptionFacets.map(_.slipNodeRep())
      println("Slipnet BondFromTo will send response")

      sender() ! BondFromToSlipnetResponse(fromFacetSlipNodeReps, toFacetSlipNodeReps)

    case SlipnetTopDownBondScout(fromdtypes, todtypes) =>
      // Partial code of WorkspaceFormulas.choose_bond_facet
      val fromob_facets = fromdtypes.map(snrep => slipNodeRefs(snrep.id)).filter(dt => bond_facets.contains(dt))
      val local_bond_facets = todtypes.map(snrep => slipNodeRefs(snrep.id)).filter(dt => fromob_facets.contains(dt))

      if (local_bond_facets.isEmpty) {
        print("no possible bond facet: Fizzle")
        sender() ! Finished
      } else {
        // We are in the middle of WorkspaceFormulas.choose_bond_facet but we need to go back to workspace
        sender() ! SlipnetTopDownBondScoutResponse(
          local_bond_facets.map(sn => sn.slipNodeRep()),
        )
      }

    case SlipnetTopDownBondScoutCategory2(bondCategory, fromDescriptor, toDescriptor, bondFacetRep) =>
      val from_descriptor = if (fromDescriptor.isEmpty) None else Some(slipNodeRefs(fromDescriptor.get.id))
      val to_descriptor = if (toDescriptor.isEmpty) None else Some(slipNodeRefs(toDescriptor.get.id))

      val descriptorDefined = from_descriptor.isDefined && to_descriptor.isDefined
      val bc1 = if (descriptorDefined) SlipnetFormulas.get_bond_category(from_descriptor.get,to_descriptor.get, identity) else None
      val bc2 = if (descriptorDefined) SlipnetFormulas.get_bond_category(to_descriptor.get,from_descriptor.get, identity) else None
      val bondFacet = slipNodeRefs(bondFacetRep.id)

      // Added test  compare to JavaCopycat
      if (bc1.isEmpty || bc2.isEmpty) {
        print("Oups Bond category empty: Fizzle")
        sender() ! Finished
      } else {
        val b1r = bc1.get
        val b2r = bc2.get

        val b1 = if (b1r == identity) sameness else b1r
        val b2 = if (b1r == identity) sameness else b2r

        val bondCat = slipNodeRefs(bondCategory)

        if ((bondCat!=b1)&&(bondCat!=b2)){
          print("no suitable link: Fizzle!");
          sender() ! Finished
        } else {
          val isFromTo = bondCat==b1
          val urgency = bondCat.bond_degree_of_association();

          bondFacet.setBuffer(100.0);
          if (from_descriptor.isDefined)
            from_descriptor.get.setBuffer(100.0);
          if (to_descriptor.isDefined)
            to_descriptor.get.setBuffer(100.0);

          sender() ! SlipnetTopDownBondScoutCategory2Response(isFromTo,urgency, bondCat.slipNodeRep(),
            left.slipNodeRep(),
            right.slipNodeRep()
          )
        }
      }
    case SlipnetTopDownBondScoutDirection2(bondCategory, fromDescriptor, toDescriptor, bondFacetRep) =>
      val from_descriptor = if (fromDescriptor.isEmpty) None else Some(slipNodeRefs(fromDescriptor.get.id))
      val to_descriptor = if (toDescriptor.isEmpty) None else Some(slipNodeRefs(toDescriptor.get.id))
      val descriptorDefined = from_descriptor.isDefined && to_descriptor.isDefined
      val bond_categoryOpt = if (descriptorDefined) SlipnetFormulas.get_bond_category(from_descriptor.get,to_descriptor.get, identity) else None
      val bondFacet = slipNodeRefs(bondFacetRep.id)
      // Added test  compare to JavaCopycat
      if (bond_categoryOpt.isEmpty) {
        print("Oups Bond category empty: Fizzle")
        sender() ! Finished
      } else {
        val bc = bond_categoryOpt.get

        val bondCategory = if (bc == identity) sameness else bc
        val urgency = bondCategory.bond_degree_of_association();
        log.debug("urgency " + urgency)
        bondFacet.setBuffer(100.0);
        if (from_descriptor.isDefined)
          from_descriptor.get.setBuffer(100.0)
        if (to_descriptor.isDefined)
          to_descriptor.get.setBuffer(100.0)

          sender() ! SlipnetTopDownBondScoutDirection2Response(urgency, bondCategory.slipNodeRep(),
            left.slipNodeRep(),
            right.slipNodeRep()
          )
      }




    // bottom-up-bond-scout codelet.java.267
    case BondFromTo2(from,to,fromDescriptor,toDescriptor, bondFacetRep) =>
      val from_descriptorOpt = fromDescriptor.map(d => slipNodeRefs(d.id))
      val to_descriptorOpt = toDescriptor.map(d => slipNodeRefs(d.id))
      val descriptorDefined = from_descriptorOpt.isDefined && to_descriptorOpt.isDefined
      val bond_categoryOpt = if (descriptorDefined) {
        log.debug(s"BondFromTo2 | descriptorDefined $descriptorDefined")
        val from_descriptor = from_descriptorOpt.get
        val to_descriptor = to_descriptorOpt.get
        log.debug(s"BondFromTo2 | from_descriptor ${from_descriptor}")
        log.debug(s"BondFromTo2 | to_descriptor ${to_descriptor}")
        //from_descriptor.outgoing_links.find(l => l.to_node == to_descriptor).map(_.label)
        log.debug(s"BondFromTo2 | fromnode.outgoing_links ${from_descriptor.outgoing_links}")
        log.debug(s"BondFromTo2 | fromnode.outgoing_links ${from_descriptor.outgoing_links.last.to_node}")

        SlipnetFormulas.get_bond_category(from_descriptor,to_descriptor, identity)
      } else None

      bond_categoryOpt match {
        case None =>
          log.debug("BondFromTo2 | no suitable link - fizzle")
          sender() ! Finished
        case Some(bondCategory) =>
          val adaptedBondCategory = if (bondCategory==identity) sameness else bondCategory
          // there is a possible bond, so propose it
          log.info(s"proposing ${adaptedBondCategory.name} bond ")
          // coderack.propose_bond(fromob,toob,bond_category,bond_facet,from_descriptor, to_descriptor,this);
          // coderack.java.274
          val bondFacet = slipNodeRefs(bondFacetRep.id)
          bondFacet.setBuffer(100.0)
          if (from_descriptorOpt.isDefined)
            from_descriptorOpt.get.setBuffer(100.0)
          if (to_descriptorOpt.isDefined)
            to_descriptorOpt.get.setBuffer(100.0)

          sender() ! BondFromTo2Response(
            adaptedBondCategory.slipNodeRep(),
            adaptedBondCategory.bond_degree_of_association(),
            left.slipNodeRep(),
            right.slipNodeRep()
          )
      }

    // codelet.java.1233
    case SlipnetBottomUpCorrespondenceScout(obj1, obj2, temperature) =>
      printDescription(obj1)
      val obj1Descriptions = obj1.descriptions.map(inflatedDescriptionRep)
      val obj2Descriptions = obj2.descriptions.map(inflatedDescriptionRep)
      val obj1Relevant_descriptions = relevant_descriptions(obj1Descriptions)
      val obj2Relevant_descriptions = relevant_descriptions(obj2Descriptions)

      // get the posible concept-mappings
      val concept_mapping_list = ConceptMapping.get_concept_mapping_list(
        obj1, obj2,
        obj1Relevant_descriptions, obj2Relevant_descriptions, slipnetInfo)

      // check the slippability of concept mappings
      log.debug("check the slippability of concept mappings");
      val cm_possible = concept_mapping_list.find(cm => {
        val slip_prob = WorkspaceFormulas.temperature_adjusted_probability(cm.slipability() / 100.0, temperature)
        WorkspaceFormulas.flip_coin(slip_prob)
      }).isDefined
      log.debug(s"End of check the slippability of concept mappings $cm_possible");

      if (concept_mapping_list.isEmpty) {
        // no possible mappings
        log.debug("no possible mappings exist: fizzle")
        sender() ! Finished

      } else {
        if (!cm_possible) {
          //cannot make necessary slippages
          log.debug("cannot make appropriate slippage: fizzle");
          sender() ! Finished

        } else {
          //find out if any are distinguishing
          val distinguishing_mappings = concept_mapping_list.filter(cm => {
            val distinguishing = cm.distinguishing(log)
            log.debug("concept_mapping " + cm + " distinguishing " + distinguishing)
            distinguishing
          })

          if (distinguishing_mappings.isEmpty) {
            // no distinguishing mappings
            log.debug("no distinguishing mappings found: fizzle");
            sender() ! Finished

          } else {
            val conceptMappingReps = concept_mapping_list.map(_.conceptMappingRep())
            val distinguishing_mappingsReps = distinguishing_mappings.map(_.conceptMappingRep())
            self.forward(CheckOnBothObjectsSpanningForStringDescriptionFlip(
              conceptMappingReps,
              distinguishing_mappingsReps,
              obj1,
              obj2
            ))
          }
        }
      }

    case  CheckOnBothObjectsSpanningForStringDescriptionFlip(conceptMappingReps,distinguishingMappingReps, obj1, obj2) =>
      val concept_mapping_list = ConceptMapping.conceptMappingsWithReps(conceptMappingReps)
      val distinguishing_mappings = ConceptMapping.conceptMappingsWithReps(distinguishingMappingReps)
      // if both objects span the strings, check to see if the
      // string description needs to be flipped
      val possible_opp_mappings = distinguishing_mappings.filter(cm => {
        (cm.description_type1 == string_position_category) &&
          (cm.description_type1 != bond_facet)
      })

      val dt1 = possible_opp_mappings.map(cm => cm.description_type1)
      val dt1ContainsCategory = dt1.contains(direction_category)
      val allOppositeMappings = ConceptMapping.all_opposite_mappings(possible_opp_mappings, opposite)


      val flip_obj2 = obj1.spans_string &&
        obj2.spans_string &&
        dt1ContainsCategory &&
        allOppositeMappings &&
        opposite.activation != 100.0

      if (flip_obj2) {
        val futureGroupOpt = groupFlipped_version(obj2.asGroupRep.get)
        if (futureGroupOpt.isEmpty) {
          sender() ! Finished
        } else {
          val futureGroup = futureGroupOpt.get
          sender() ! ProposeAnyCorrespondenceSlipnetResponse(futureGroup)

        }
      } else {
        println("Proposing correspondence with concept mappings:");
        for(cm <- concept_mapping_list) {
          println(cm.toString())
        }

        // Coderack.java.323 (propose_correspondence)
        activateConceptMappingList(concept_mapping_list)

        val dcm = distinguishing_concept_mappings(concept_mapping_list)
        val totalStrength = dcm.map(_.strength()).sum

        val conceptMappingReps = concept_mapping_list.map(cm => cm.conceptMappingRep())
        sender() ! ProposeAnyCorrespondenceSlipnetResponse2(
          obj1,
          obj2,
          conceptMappingReps,
          true,
          dcm.size,
          totalStrength
        )
      }


    case SlipnetBottomUpCorrespondenceScout2(obj1, obj2) =>
      val obj1Descriptions = obj1.descriptions.map(inflatedDescriptionRep)
      val obj2Descriptions = obj2.descriptions.map(inflatedDescriptionRep)
      val obj1Relevant_descriptions = relevant_descriptions(obj1Descriptions)
      val obj2Relevant_descriptions = relevant_descriptions(obj2Descriptions)

      val newObj2Descriptions = obj2.descriptions.map(inflatedDescriptionRep)
      val newObj2Relevant_descriptions = relevant_descriptions(newObj2Descriptions)

      val concept_mapping_list = ConceptMapping.get_concept_mapping_list(
        obj1, obj2,
        obj1Relevant_descriptions, obj2Relevant_descriptions, slipnetInfo);

      println("Proposing correspondence with concept mappings:");
      for(cm <- concept_mapping_list) {
        println(cm.toString())
      }

      // Coderack.java.323 (propose_correspondence)
      activateConceptMappingList(concept_mapping_list)

      val dcm = distinguishing_concept_mappings(concept_mapping_list)
      val totalStrength = dcm.map(_.strength()).sum

      val conceptMappingReps = concept_mapping_list.map(cm => cm.conceptMappingRep())
      sender() ! ProposeAnyCorrespondenceSlipnetResponse2(
        obj1,
        obj2,
        conceptMappingReps,
        true,
        dcm.size,
        totalStrength
      )







    /*case ProposeAnyCorrespondence2(obj1, obj2, codelet) =>
      val obj1Descriptions = obj1.descriptions.map(inflatedDescriptionRep)
      val obj2Descriptions = obj2.descriptions.map(inflatedDescriptionRep)
      val obj1Relevant_descriptions = relevant_descriptions(obj1Descriptions)
      val obj2Relevant_descriptions = relevant_descriptions(obj2Descriptions)

      val slipnetInfo = SlipnetInfo(
        opposite,
        sameness,
        identity,
        letter,
        group,
        whole,
        slipnet_numbers.toList
      )

      // get the posible concept-mappings
      val concept_mapping_list = ConceptMapping.get_concept_mapping_list(
        obj1, obj2,
        obj2Relevant_descriptions, obj2Relevant_descriptions, slipnetInfo);


      coderack ! ProposeCorrespondence2(obj1,obj2,concept_mapping_list,flip_obj2,this);*/
    case SlipnetGoWithBottomUpDescriptionScout(d, t) =>
      val chosen_descriptor = slipNodeRefs(d.id)
      val hpl = similar_has_property_links(chosen_descriptor, t)
      if (hpl.isEmpty) {
        // no has property links
        log.debug("has no property links: Fizzle!");
        sender() ! Finished

      } else {
        val v = hpl.map(sl => sl.degree_of_association() * sl.to_node.activation )
        val chosen: SlipnetLink = hpl(Utilities.valueProportionalRandomIndexInValueList(v))
        val chosen_property = chosen.to_node

        val chosen_propertyRep = chosen_property.slipNodeRep()
        val dtOpt = chosen_property.category()
        dtOpt match {
          case Some(cat) =>
            sender() ! SlipnetGoWithBottomUpDescriptionScoutResponse(cat.slipNodeRep(), chosen_propertyRep)

          case None =>
            sender() ! Finished
        }
      }

    case SlipnetGoWithTopDownDescriptionScout(chosen_object, descriptonTypeID) =>
      log.debug("SlipnetGoWithTopDownDescriptionScout")
      val description_type = slipNodeRefs(descriptonTypeID)
      val info = get_description_type_instance_links_to_node_info(description_type);
      sender() ! SlipnetGoWithTopDownDescriptionScoutResponse(info)

    case SlipnetGoWithTopDownDescriptionScout2(cp) =>
      log.debug("GoWithTopDownDescriptionScoutResponse2")
      val chosen_property = slipNodeRefs(cp.id)
      chosen_property.category() match {
        case Some(cat) =>
          sender() ! SlipnetGoWithTopDownDescriptionScoutResponse2(cat.slipNodeRep())

        case None =>
          sender() ! Finished
      }

    case SlipnetGoWithTopDownGroupScoutCategory(groupID, temperature) =>
      val group_cat = slipNodeRefs(groupID)
      log.debug("trying to build "+group_cat.id + " group");

      val bond_categoryOpt = SlipnetFormulas.get_related_node(group_cat, bond_category, identity)
      bond_categoryOpt match {
        case Some(bond_category) =>
          log.debug("top-down-group-scout--category bond_category " + bond_category)
          sender() ! SlipnetGoWithTopDownGroupScoutCategoryResponse(bond_category.slipNodeRep(), groupSlipnetInfo())
        case None =>
          log.debug("<c> no bond-category found")
          sender() ! Finished

      }
    case SlipnetGoWithTopDownGroupScoutCategory2(group_cat_id, dir) =>
      val group_category = slipNodeRefs(group_cat_id)
      val mydirection: SlipNodeRep = dir match {
        case None =>
          val v = List(left.activation, right.activation)
          if (Utilities.valueProportionalRandomIndexInValueList(v) == 0) left.slipNodeRep() else right.slipNodeRep()
        case Some(value) => value
      }
      log.debug("mydirection " + mydirection);

      //print("trying from "+fromob+" "+bond_category.pname+" checking to "+direction.pname+" first");
      sender() ! SlipnetGoWithTopDownGroupScoutCategory2Response(group_category.slipNodeRep(), mydirection)

    case SlipnetGoWithTopDownGroupScoutDirection0 =>
      sender() ! SlipnetGoWithTopDownGroupScoutDirection0Response(groupSlipnetInfo())


    case SlipnetGoWithTopDownGroupScoutDirection(bond_category_rep, first_bond, fromob) =>
      val bondCategory = slipNodeRefs(bond_category_rep.id)
      val groupCategoryOpt = SlipnetFormulas.get_related_node(bondCategory,group_category, identity)
      groupCategoryOpt match {
        case Some(groupCategory) =>
          log.debug("trying from "+fromob+" "+bondCategory);
          workspace.forward(CommonSubProcessing(groupCategory.slipNodeRep(), fromob,first_bond, bondCategory.slipNodeRep()))

        case None =>
          log.debug("Oups group_category is null")
          sender() ! Finished

      }


    // Coderack.java.292, propose_group
    case CompleteProposeGroup(grCategoryRep, dirCategoryRepOpt) =>
      log.debug("CompleteProposeGroup")
      val grCategory = slipNodeRefs(grCategoryRep.id)
      val bond_categoryOpt = SlipnetFormulas.get_related_node(grCategory,bond_category, identity)
      // match added compare to JavaCopycat
      bond_categoryOpt match {
        case Some(bond_category) =>
          bond_category.setBuffer(100.0)
          dirCategoryRepOpt match {
            case Some(dirCategoryRep) =>
              val dirCategory = slipNodeRefs(dirCategoryRep.id)
              dirCategory.setBuffer(100.0)
            case None =>
          }
          val urgency = bond_category.bond_degree_of_association()

          sender() ! CompleteProposeGroupResponse(urgency, bond_category.slipNodeRep(), grCategoryRep)

        case None =>
          log.debug("<c> no bond-category found")
          sender() ! Finished
      }
    case GetRelatedNodeOf(slipnodeID: String, lookingAtSlipNodeID: String) =>
      log.debug("GetRelatedNodeOf")
      val fromSlipNode = slipNodeRefs(slipnodeID)
      val slipNode = slipNodeRefs(lookingAtSlipNodeID)
      val related = SlipnetFormulas.get_related_node(fromSlipNode,slipNode,identity);
      sender() ! GetRelatedNodeOfResponse(related.map(_.slipNodeRep()))

    case SlipnetGoWithGroupScoutWholeString(bc) =>
      val bond_cat = slipNodeRefs(bc.id)

      val related = SlipnetFormulas.get_related_node(bond_cat,group_category, identity)
      log.debug("SlipnetGoWithGroupScoutWholeString " + related)
      sender() ! SlipnetGoWithGroupScoutWholeStringResponse(related.map(_.slipNodeRep()),groupSlipnetInfo())

    case GetLeftAndRight =>
      sender() ! GetLeftAndRightResponse(left.slipNodeRep(), right.slipNodeRep(), groupSlipnetInfo())


    case SlipnetGoWithGroupStrengthTester(group_category_id: String) =>
      val group_cat = slipNodeRefs(group_category_id)
      val related = SlipnetFormulas.get_related_node(group_cat,bond_category, identity)
      sender() ! SlipnetGoWithGroupStrengthTesterResponse(related.get.degree_of_association())

    case SlipnetGoWithGroupStrengthTester2(g: GroupRep, strength: Double) =>
      log.debug("SlipnetGoWithGroupStrengthTester2")
      // TODO same code as above
      val group_cat = slipNodeRefs(g.group_category.id)
      val related = SlipnetFormulas.get_related_node(group_cat,bond_category, identity)
      if (related.isDefined)
        related.get.setBuffer(100.0)
      if (g.direction_category.isDefined) {
        val direction_cat = slipNodeRefs(g.direction_category.get.id)
        direction_cat.setBuffer(100.0)
      }
      // GUI workspace.Workspace_Comments.text+=": succeeded ";
      // GUI if (!coderack.remove_terraced_scan) workspace.WorkspaceArea.AddObject(g,2);
      sender() ! SlipnetGoWithGroupStrengthTesterResponse2

    case SlipnetLookAHeadForNewBondCreation(s: ActorRef, g, index: Int, incg, newBondList, from_obj_id, to_obj_id) =>
      val group_cat = slipNodeRefs(g.group_category.id)
      val related = SlipnetFormulas.get_related_node(group_cat,bond_category, identity)
      val bond_facet = slipNodeRefs(g.bond_facet.id)
      sender() ! SlipnetLookAHeadForNewBondCreationResponse(s, g.uuid, index, incg, newBondList, related.map(_.slipNodeRep()),
        from_obj_id, to_obj_id, bond_facet.slipNodeRep(), left.slipNodeRep(), right.slipNodeRep())


    case SlipnetProposeRule(d, r) =>
      val description = if (d.isDefined) Some(slipNodeRefs(d.get.id)) else None
      val relation = if (r.isDefined) Some(slipNodeRefs(r.get.id)) else None
      val urgency = if (description.isDefined && relation.isDefined) {
        (Math.sqrt((description.get.conceptual_depth + relation.get.conceptual_depth)/200.0))*100.0
      } else 100.0 // My guess !
      sender() ! SlipnetProposeRuleResponse(
        urgency,
        length.slipNodeRep(),
        predecessor.slipNodeRep(),
        successor.slipNodeRep()
      )

    case SlipnetGoWithRuleScout =>
      log.debug("SlipnetGoWithRuleScout")
      sender() ! SlipnetGoWithRuleScoutResponse(string_position_category.slipNodeRep(), letter_category.slipNodeRep())

    case SlipnetCompleteSlippageList(slippagesShell: SlippageListShell) =>
      log.debug("SlipnetCompleteSlippageList")
      val sl = slippagesShell.sl.map(cm => ConceptMapping.conceptMappingRefs(cm.uuid))
      val slippageCandidates = ConceptMapping.conceptMappingsWithReps(slippagesShell.slippageCandidates)
      val filteredSlippageCandidates = slippageCandidates.filter(cm => cm.slippage())

      val slippage_list = slippage_list_accumulation(sl, filteredSlippageCandidates)
      log.debug("did slippage_list: ");

      for(slippage <- slippage_list) {
        print(s"slippage ${slippage.toString()}")
      }

      val slippage_list_rep = slippage_list.map(cm => cm.conceptMappingRep())
      sender() ! SlipnetCompleteSlippageListResponse(slippage_list_rep)

    case SlipnetGoWithRuleScout3(ol, changedReplacementRelation, letterCategory, t) =>
      log.debug("SlipnetGoWithRuleScout3 choosing a description based on conceptual depth:" + letterCategory + " with chanded replacement relation " + changedReplacementRelation)
      val descriptor = {
        val object_list = ol.map(sr => slipNodeRefs(sr.id))
        chooseSlipNodeWithTemperature(object_list,t)
      }
      log.debug("Chosen descriptor: "+descriptor.id)

      // choose the relation(change the letmost object to..xxxx) i.e. "successor" or "d"

      log.debug("choosing relation based on conceptual depth:" + letterCategory)
      val relation = {
        val letterCategorySlipNode = slipNodeRefs(letterCategory.id)
        val object_list = if (changedReplacementRelation.isDefined) {
          val relation = slipNodeRefs(changedReplacementRelation.get)
          List(relation, letterCategorySlipNode)
        } else List(letterCategorySlipNode)

        object_list.foreach(sln => {
          log.debug("Rule scout object_list slipnode " + sln.conceptual_depth + " slipnode " + sln)
        })
        chooseSlipNodeWithTemperature(object_list,t)
      }
      log.debug("Chosen relation: "+relation.id);

      log.debug("proposing rule:");
      log.debug("change letter-cat of "+descriptor.id+" letter to "+ relation.id);

      sender() ! RuleScoutProposeRule(
        Some(letter_category.slipNodeRep()),
        Some(descriptor.slipNodeRep()),
        Some(letter.slipNodeRep()),
        Some(relation.slipNodeRep())
      )

      // Codelet.java.1327
    case SlipnetGoWithImportantObjectCorrespondenceScout(rds, t) =>
      log.debug("SlipnetGoWithImportantObjectCorrespondenceScout")
      val relevantDescriptors = rds.map(rd => slipNodeRefs(rd.id))
      val sOpt = choose_slipnode_by_conceptual_depth(relevantDescriptors,t)
      log.debug("choose_slipnode_by_conceptual_depth " + sOpt)
      sender() ! SlipnetGoWithImportantObjectCorrespondenceScoutResponse(sOpt.map(_.slipNodeRep()))

    // Codelet.java.1368
    case SlipnetGoWithImportantObjectCorrespondenceScout2(obj1, obj2, obj2GroupRep, temperature) =>
      log.debug("SlipnetGoWithImportantObjectCorrespondenceScout2")
      val obj1Descriptions = obj1.descriptions.map(inflatedDescriptionRep)
      val obj2Descriptions = obj2.descriptions.map(inflatedDescriptionRep)
      val obj1Relevant_descriptions = relevant_descriptions(obj1Descriptions)
      val obj2Relevant_descriptions = relevant_descriptions(obj2Descriptions)

      // get the posible concept-mappings
      log.debug("get the posible concept-mappings with obj1Relevant_descriptions " + obj1Relevant_descriptions);

      val concept_mapping_list = ConceptMapping.get_concept_mapping_list(
        obj1, obj2,
        obj1Relevant_descriptions, obj2Relevant_descriptions, slipnetInfo)

      log.debug("check the slippability of concept mappings " + concept_mapping_list.size);

      if (concept_mapping_list.isEmpty) {
        // no possible mappings
        sender() ! Finished
      } else {
        // check the slippability of concept mappings
        var cm_possible = false;

        log.debug("will cm_possible " + temperature);
        // cannot use find because we need to stick to the original
        for(cm <- concept_mapping_list) {
          log.debug("cm.slipability " + cm.slipability);

          val slip_prob = WorkspaceFormulas.temperature_adjusted_probability(cm.slipability() / 100.0, temperature)
          if (WorkspaceFormulas.flip_coin(slip_prob)) cm_possible = true
        }
        log.debug("cm_possible " + cm_possible);

        if (!cm_possible) {
          //cannot make necessary slippages
          sender() ! Finished
        } else {
          //find out if any are distinguishing
          val distinguishing_mappings = concept_mapping_list.filter(cm => {
            cm.distinguishing(log)
          })
          if (distinguishing_mappings.isEmpty) {
            // no distinguishing mappings
            sender() ! Finished
          } else {
            // if both objects span the strings, check to see if the
            // string description needs to be flipped
            val conceptMappingReps = concept_mapping_list.map(_.conceptMappingRep())
            val distinguishing_mappingsReps = distinguishing_mappings.map(_.conceptMappingRep())
            self.forward(CheckOnBothObjectsSpanningForStringDescriptionFlip(
              conceptMappingReps,
              distinguishing_mappingsReps,
              obj1,
              obj2
            ))
          }
        }
      }





    case GroupFlippedVersion(obj: GroupRep) =>
      val flipped = groupFlipped_version(obj)
      sender() ! GroupFlippedVersionResponse(flipped)

// Codelet.java.1495
    case SlipnetGoWithCorrespondenceBuilder(conceptMappingReps) =>
      log.debug("Slipnet. SlipnetGoWithCorrespondenceBuilder")
      val existingConceptMappingList = ConceptMapping.conceptMappingsWithReps(conceptMappingReps)
      for (cm <- existingConceptMappingList) {
        if (cm.label.isDefined) cm.label.get.setBuffer(100.0)
      }
      val updatedCorrespondenceCMs = existingConceptMappingList.filter(cm => {
        !(cm.concept_mapping_present(existingConceptMappingList))
      })
      val updatedCorrespondenceCMReps = updatedCorrespondenceCMs.map(_.conceptMappingRep())
      sender() ! SlipnetGoWithCorrespondenceBuilderResponse(updatedCorrespondenceCMReps)

    case SlipnetGoWithCorrespondenceBuilder2(correspondence, correspondenceReps, workspaceCorrespondences) =>
      log.debug("Slipnet. SlipnetGoWithCorrespondenceBuilder2")
      val incc = get_incompatible_correspondences(correspondence, correspondenceReps)
      val (internal_strength: Double, comap: Map[String, Boolean]) = correpondenceData(correspondence, workspaceCorrespondences)

      sender() ! SlipnetGoWithCorrespondenceBuilderResponse2(incc, internal_strength, comap)

    case SlipnetGoWithCorrespondenceBuilder3(correspondence, incompatible_bond_base, workspaceCorrespondences) =>
      log.debug("Slipnet. SlipnetGoWithCorrespondenceBuilder3")
      val b = get_incompatible_bond(correspondence, incompatible_bond_base)

      val (internal_strength: Double, comap: Map[String, Boolean]) = correpondenceData(correspondence, workspaceCorrespondences)
      val bond_degree_of_association = b.map(br => {
        val bondCategorySlipNode = slipNodeRefs(br.bondCategorySlipNodeID)
        bondCategorySlipNode.bond_degree_of_association()
      })

      sender() ! SlipnetGoWithCorrespondenceBuilderResponse3(b, internal_strength, comap, bond_degree_of_association)

    case SlipnetGoWithCorrespondenceBuilder4(c) =>
      log.debug("Slipnet. SlipnetGoWithCorrespondenceBuilder4")
      // add mappings to accessory-concept-mapping-list
      val v = relevant_distinguishing_cms(c)
      val accessory_concept_mapping_list = v.filter(cm => cm.slippage()).map(_.symmetric_version())

      sender() ! SlipnetGoWithCorrespondenceBuilder4Response(accessory_concept_mapping_list.map(_.conceptMappingRep()))

    case SlipnetGoWithCorrespondenceBuilder5(groupObjs) =>
      val accessory_concept_mapping_list = groupObjs match {
        case Some(params) =>
          val ds1 = params.ds1.map(d => convertDescriptionRepToInflatedDescriptionRep(d))
          val ds2 = params.ds2.map(d => convertDescriptionRepToInflatedDescriptionRep(d))
          val cmv = ConceptMapping.get_concept_mapping_list(
            params.w1,
            params.w2,
            ds1,
            ds2,
            slipnetInfo()
          )
          cmv ::: cmv.filter(cm => cm.slippage()).map(_.symmetric_version())
        case None => List.empty[ConceptMapping]
      }
      sender() ! SlipnetGoWithCorrespondenceBuilder5Response(accessory_concept_mapping_list.map(_.conceptMappingRep()))

    case SlipnetGoWithCorrespondenceBuilder6(cms) =>
      val conceptMappings = ConceptMapping.conceptMappingsWithReps(cms)
      log.debug("SlipnetGoWithCorrespondenceBuilder6 cms " + conceptMappings)

      for (cm <- conceptMappings) {
        if (cm.label.isDefined) cm.label.get.setActivation(100.0)
      }
      sender() ! Finished

    case SlipnetGoWithBondStrengthTester(bondRep) =>
      log.debug("SlipnetGoWithBondStrengthTester")
      val bondCategorySlipNode = slipNodeRefs(bondRep.bondCategorySlipNodeID)
      sender() ! SlipnetGoWithBondStrengthTesterResponse(bondCategorySlipNode.bond_degree_of_association())

    case SlipnetGoWithCorrespondenceStrengthTester(c: CorrespondenceRep, workspaceCorrespondences) =>
      val (internal_strength: Double, comap: Map[String, Boolean]) = correpondenceData(c, workspaceCorrespondences)
      sender() ! SlipnetGoWithCorrespondenceStrengthTesterResponse(internal_strength, comap)

    case SlipnetGoWithCorrespondenceStrengthTester2(c: CorrespondenceRep) =>
      // activate some concepts
      for (cm <- c.concept_mapping_list){
        slipNodeRefs(cm.description_type1.id).setBuffer(100.0);
        slipNodeRefs(cm.descriptor1.id).setBuffer(100.0);
        slipNodeRefs(cm.description_type2.id).setBuffer(100.0);
        slipNodeRefs(cm.descriptor2.id).setBuffer(100.0);
      }
      sender() ! SlipnetGoWithCorrespondenceStrengthTester2Response


    case DataForStrengthUpdate(brs, crs, t) =>
      log.debug("Slipnet. DataForStrengthUpdate")

      val bondData = brs.map(br => {
        val bondCategorySlipNode = slipNodeRefs(br.bondCategorySlipNodeID)
        (br.uuid, bondCategorySlipNode.bond_degree_of_association())
      }).toMap

      val correspondenceData = crs.map(cr => {
        val relevant_dcms = relevant_distinguishing_cms(cr)
        System.out.println("correspondence_internal_strength relevant_dcms " + relevant_dcms);

        val internal_strength = correspondence_internal_strength(relevant_dcms)

        val cocouples  = for (co <- crs) yield (co.uuid,  supporting_correspondences(cr, co))
        val comap = cocouples.toMap

        (cr.uuid, CorrespondenceUpdateStrengthData(internal_strength, comap))
      }).toMap
      //val correspondenceData = Map.empty[String,CorrespondenceUpdateStrengthData]
      sender() ! DataForStrengthUpdateResponse(bondData, correspondenceData,t)

    case PostTopBottomCodeletsGetInfo(
        t: Double,
        intra_string_unhappiness: Double,
        inter_string_unhappiness: Double,
        ruleTotalWeaknessOpt: Option[Double],
        number_of_bonds: Int,
        unrelated_objects_size: Int,
        ungrouped_objects_size: Int,
        unreplaced_objects_size: Int,
        uncorresponding_objects_size: Int,
        codeletsSize: Int
    ) =>
      var codeletToPost = ListBuffer.empty[(String,Either[Double, Int], Option[String], Option[Double])]
      var codeletsCount = codeletsSize
      for (s <- slipNodes) {
        if (s.activation == 100.0) {
          for (st <- s.codelets) {
            log.debug(">>>> post_top_down_codelets " + st);

            val prob = get_post_codelet_probability(st, t, intra_string_unhappiness, inter_string_unhappiness, unreplaced_objects_size, ruleTotalWeaknessOpt);
            val num = get_num_codelets_to_post(st, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
            log.debug("prob " + prob + " num " + num);
            for (t <- 1 to num) {

              if (Random.rnd(log) < prob) {
                val rawUrgency = s.activation * s.conceptual_depth / 100.0


                log.debug("post_top_down_codelets rawUrgency " + rawUrgency + " count " + codeletsCount);

                log.debug("create codelet " + st);

                // Simulate Aadd of the codelet to codelets
                codeletsCount = codeletsCount + 1

                log.debug("Post Codelet codelets size " + codeletsCount);

                val rnd = if (codeletsCount > 100) {
                  log.debug("Post. codelets.size > 100");

                  Some(Random.rnd(null))
                } else None


                val tuple = (st, Left(rawUrgency), Some(s.slipNodeRep().id), rnd)
                codeletToPost += tuple
              }
              log.debug("<<<<");
              log.debug("");
            }
          }
        }
      }
      log.debug("------------------------ post_bottom_up_codelets");
      val f = () => { codeletsCount = codeletsCount + 1 }
      codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.BottomUpDescriptionScout,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.BottomUpBondScout,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.GroupScoutWholeString,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.BottomUpCorrespondenceScout,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.ImportantObjectCorrespondenceScout,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.ReplacementFinder,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.RuleScout,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.RuleTranslator,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      if (!remove_breaker_codelets) codeletToPost ++= get_bottom_up_codelets(f,codeletsCount,CodeletTypeString.Breaker,t,intra_string_unhappiness,inter_string_unhappiness, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
      log.debug("post_bottom_up_codelets codeletToPost " + codeletToPost);

      workspace ! PostTopBottomCodeletsGetInfoResponse(codeletToPost.toList, t)

    case PrepareBondFighting(groupID, bondReps) =>
      log.debug("PrepareBondFighting")
      val defOfAssos = bondReps.map(bondRep => {
        val bondCategorySlipNode = slipNodeRefs(bondRep.bondCategorySlipNodeID)
        (bondRep.uuid, bondCategorySlipNode.bond_degree_of_association())
      }).toMap
      sender() ! PrepareBondFightingResponse(bondReps, defOfAssos)

    case SlipnetGoWithGroupBuilder(group_category_id) =>
      val grCategory = slipNodeRefs(group_category_id)
      val bond_categoryOpt = SlipnetFormulas.get_related_node(grCategory,bond_category, identity)
      bond_categoryOpt match {
        case Some(bond_category) =>
          sender() ! SlipnetGoWithGroupBuilderResponse(bond_category.degree_of_association())
        case None =>
          log.debug("SlipnetGoWithGroupBuilder. SlipnetFormulas.get_related_node null")
          sender() ! Finished
      }

    case SlipnetPrepareGroupFighting(group_category_id, incg: List[String], group_category_id_by_group_id) =>
      val grCategory = slipNodeRefs(group_category_id)
      val bond_categoryOpt = SlipnetFormulas.get_related_node(grCategory,bond_category, identity)
      bond_categoryOpt match {
        case Some(bc) =>
          val degree_of_association1 = bc.degree_of_association()
          val degree_of_association2Tuples = incg.map(groupID => {
            val grCategory2 = slipNodeRefs(group_category_id_by_group_id(groupID))
            val bond_categoryOpt2 = SlipnetFormulas.get_related_node(grCategory2,bond_category, identity)
            bond_categoryOpt2 match {
              case Some(bond_category2) => Some((groupID, bond_category2.degree_of_association()))
              case None => None
            }
          })
          val degree_of_association2TuplesFlatten = degree_of_association2Tuples.flatten
          if (degree_of_association2Tuples.size != degree_of_association2TuplesFlatten.size) {
            log.debug("SlipnetPrepareGroupFighting. SlipnetFormulas.get_related_node null")
            sender() ! Finished
          } else {
            sender() ! SlipnetPrepareGroupFightingResponse(bc.degree_of_association(), degree_of_association2TuplesFlatten.toMap)
          }
        case None =>
          log.debug("SlipnetPrepareGroupFighting. SlipnetFormulas.get_related_node null")
          sender() ! Finished
      }


  }

  private def correpondenceData(c: CorrespondenceRep, workspaceCorrespondences: List[CorrespondenceRep]) = {
    val relevant_dcms = relevant_distinguishing_cms(c)
    log.debug("relevant_dcms " + relevant_dcms.size)
    val internal_strength = correspondence_internal_strength(relevant_dcms)

    val cocouples = for (co <- workspaceCorrespondences) yield (co.uuid, supporting_correspondences(c, co))
    val comap = cocouples.toMap
    (internal_strength, comap)
  }

  def get_bottom_up_codelets(f: () => Unit,
                             codeletsCount: Int,
                             st: String,
                             temperature: Double,
                             intra_string_unhappiness: Double,
                             inter_string_unhappiness: Double,
                             ruleTotalWeaknessOpt: Option[Double],
                             number_of_bonds: Int,
                             unrelated_objects_size: Int,
                             ungrouped_objects_size: Int,
                             unreplaced_objects_size: Int,
                             uncorresponding_objects_size: Int
                            ): ListBuffer[(String,Either[Double, Int], Option[String], Option[Double])] = {
    log.debug("get_bottom_up_codelets " + st);

    val prob = get_post_codelet_probability(st, temperature, intra_string_unhappiness, inter_string_unhappiness, unreplaced_objects_size, ruleTotalWeaknessOpt);
    var num = get_num_codelets_to_post(st, ruleTotalWeaknessOpt, number_of_bonds, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)

    log.debug("get_bottom_up_codelets prob " + prob + " num " + num);

    if ((speed_up_bonds)&&(st.indexOf("bond") > -1)) num*=3;
    if ((speed_up_bonds)&&(st.indexOf("group") > -1)) num*=3;

    var codeletToPost = ListBuffer.empty[(String,Either[Double, Int], Option[String], Option[Double])]
    var count = codeletsCount

    for (t <- 1 to num){
      var urgency = 3;
      if (st.equals("breaker")) urgency = 1;
      if ((temperature < 25.0) && (st.indexOf("translator")> -1)) urgency=5;
      val rnd = Random.rnd(null)
      log.debug(">>>> get_bottom_up_codelets " + st + " rnd " + rnd + " prob " + prob + " t " + temperature + " index " + st.indexOf("translator"));

      if (rnd < prob){


        log.debug("get_bottom_up_codelets rawUrgency" + urgency + " count " + count);

        f()
        count = count + 1

        log.debug("Post Codelet codelets size " + count);

        val rnd = if (count > 100) {
          log.debug("Post. codelets.size > 100");

          Some(Random.rnd(null))
        } else None


        val tuple = (st, Right(urgency), None, rnd)

        codeletToPost += tuple
      }
      log.debug("<<<<");
      log.debug("");
    }
    //log.debug("get_bottom_up_codelets 2 post_bottom_up_codelets codeletToPost " + codeletToPost);

    codeletToPost
  }
// 1722

  def get_num_codelets_to_post(structure_category: String,
                               ruleTotalWeaknessOpt : Option[Double],
                               number_of_bonds: Int,
                               unrelated_objects_size: Int,
                               ungrouped_objects_size: Int,
                               unreplaced_objects_size: Int,
                               uncorresponding_objects_size: Int
                              ): Int = {
    if (structure_category.equals("breaker")) 1
    else if (structure_category.indexOf("description") > -1) 1
    else if (structure_category.indexOf("translator") > -1) {
      ruleTotalWeaknessOpt match {
        case Some(ruleTotalWeakness) => 1
        case None => 0
      }
    } else if (structure_category.indexOf("rule") > -1) 2
    else if ((structure_category.indexOf("group") > -1) && (number_of_bonds == 0)) 0
    else if ((structure_category.indexOf("replacement") > -1) && (ruleTotalWeaknessOpt.isDefined)) 0
    else rough_num_of_objects(structure_category, unrelated_objects_size, ungrouped_objects_size, unreplaced_objects_size, uncorresponding_objects_size)
  }


  def rough_num_of_objects(structure_category: String,
                           unrelated_objects_size: Int,
                           ungrouped_objects_size: Int,
                           unreplaced_objects_size: Int,
                           uncorresponding_objects_size: Int
                          ): Int = {
    // bond -> unrelated_objects
    // group -> ungrouped_objects
    // replacement -> unreplaced
    // correspondence-> uncorresponding

    // number of objects of the specified type in the workspace
    val n = if (structure_category.indexOf("bond") > -1) {
      log.debug("rough_num_of_objects. bond " + unrelated_objects_size);

      unrelated_objects_size
    }
    else if (structure_category.indexOf("group") > -1) ungrouped_objects_size
    else if (structure_category.indexOf("replacement") > -1) unreplaced_objects_size
    else if (structure_category.indexOf("correspondence") > -1) uncorresponding_objects_size
    else 0

    val d: Double = n.toDouble
    if (d < Formulas.blur(log,2.0)) return 1
    if (d< Formulas.blur(log,4.0)) return 2
    3
  }

  def get_post_codelet_probability(structure_category: String,
                                   t: Double,
                                   intra_string_unhappiness: Double,
                                   inter_string_unhappiness: Double,
                                   unreplaced_objects_size: Double,
                                   ruleTotalWeaknessOpt: Option[Double]
                                  ): Double = {
    if (structure_category.equals("breaker")) {
//      log.debug("breaker")
      1.0
    } else if (structure_category.indexOf("description") > -1) {
//      log.debug("description")
      (t/100.0)*(t/100.0)
    } else if (structure_category.indexOf("correspondence") > -1) {
//      log.debug("correspondence " + inter_string_unhappiness)
      inter_string_unhappiness / 100.0
    } else if (structure_category.indexOf("replacement") > -1) {  // Will never happen !
//      log.debug("replacement")
      if (unreplaced_objects_size > 0) 1.0 else 0.0
    } else if (structure_category.indexOf("rule") > -1) {
      val total_weaknessString = ruleTotalWeaknessOpt match {
        case Some(tw) => tw.toString
        case None => ""
      }
      log.debug("get_post_codelet_probability workspace rule total_weakness " + total_weaknessString)
      ruleTotalWeaknessOpt match {
        case Some(ruleTotalWeakness) => ruleTotalWeakness / 100.0
        case None => 1.0
      }
    } else if (structure_category.indexOf("translated") > -1) { // will never happen !
//      log.debug("translated")
      ruleTotalWeaknessOpt match {
        case Some(ruleTotalWeakness) => 1.0
        case None => 0.0
      }
    } else {
      //log.debug("L'autre")
      intra_string_unhappiness / 100.0
    }
  }


  def printDescription(wo: WorkspaceObjectRep) = {
    for(d <- wo.descriptions) {
      val descriptor = slipNodeRefs(d.descriptor.get.id)
      log.debug(s"Description ${descriptor.id} ${descriptor.activation}")
    }
  }

  def supporting_correspondences(
    c1: CorrespondenceRep, c2: CorrespondenceRep): Boolean = {
    // Returns t if c1 supports c2, nil otherwise.  For now, c1 is
    // defined to support c2 if c1 is not incompatible with c2, and
    // has a concept-mapping that supports the concept-mappings of c2.
    log.debug("supporting_correspondences c1 c2")

    if ((c1.obj1==c2.obj1)||(c1.obj2==c2.obj2)) {
      log.debug("(c1.obj1==c2.obj1)||(c1.obj2==c2.obj2)")
      return false
    };
    if (incompatible_correspondences(c1,c2)) {
      log.debug("incompatible_correspondences")
      return false
    };
    val c1dms = ConceptMapping.conceptMappingsWithReps(c1.concept_mapping_list)
    val c2dms = ConceptMapping.conceptMappingsWithReps(c2.concept_mapping_list)
    for (cm <- c1dms) {
      log.debug("cm1 distinguishing " + cm.distinguishing(log) + " cm " + cm);
    }
    for (cm <- c2dms) {
      log.debug("cm2 distinguishing " + cm.distinguishing(log) + " cm " + cm);
    }

    val dcm1 = distinguishing_concept_mappings(c1dms)
    val dcm2 = distinguishing_concept_mappings(c2dms)
    log.debug("dcm1 " + dcm1);
    log.debug("dcm2 " + dcm2);

    supporting_concept_mappingsWithXY(dcm1,dcm2,true)
  }


  def correspondence_internal_strength(relevant_dcms: List[ConceptMapping]): Double = {
    // a function of how many concept-mapping there are, how strong they
    // are, and how much internal coherence there is among concept mappings
    if (relevant_dcms.isEmpty) 0
    else {
      log.debug("average of the strengths of all");

      val num_of_concept_mappings = relevant_dcms.size
      // average of the strengths of all
      val sum_strength = relevant_dcms.map(_.strength()).sum
      val average_strength = sum_strength / num_of_concept_mappings.toDouble
      log.debug("num_of_concept_mappings " + num_of_concept_mappings + " sum_strength " + sum_strength + " average_strength " + average_strength)

      // returns 1.2 but should be 0.8
      val num_of_concept_mappings_factor = num_of_concept_mappings match {
        case 1 => 0.8
        case 2 => 1.2
        case _ => 1.6
      }
      log.debug("num_of_concept_mappings_factor " + num_of_concept_mappings_factor);
      // should return false
      val internal_coherence_factor = if (internally_coherent(relevant_dcms)) 2.5 else 1.0
      println(s"correspondence_internal_strength average_strength $average_strength internal_coherence_factor $internal_coherence_factor num_of_concept_mappings_factor $num_of_concept_mappings_factor")
      val rawI = average_strength * internal_coherence_factor * num_of_concept_mappings_factor
      System.out.println("calculate_internal_strength internal_strength " +  rawI);

      if (rawI > 100.0) 100.0 else rawI
    }
  }
  def supporting_concept_mappings(
                               cm1: ConceptMapping , cm2: ConceptMapping) : Boolean = {
    // Concept-mappings (a -> b) and (c -> d) support each other if a is related
    // to c and if b is related to d and the a -> b relationship is the same as the
    // c -> d relationship.  E.g., rightmost -> rightmost supports right -> right
    // and leftmost -> leftmost.  Notice that slipnet distances are not looked
    // at, only slipnet links.  This should be changed eventually.

    // If the two concept-mappings are the same, then return t.  This
    // means that letter->group supports letter->group, even though these
    // concept-mappings have no label.
    log.debug("supporting_concept_mappings");
    if ((cm1.descriptor1==cm2.descriptor1)&&(cm1.descriptor2==cm2.descriptor2)) {
      log.debug("supporting_concept_mappings1 true");
      return true
    };
    // if the descriptors are not related return false
    if (!(SlipnetFormulas.related(cm1.descriptor1,cm2.descriptor1)||
      SlipnetFormulas.related(cm1.descriptor2,cm2.descriptor2))) {
      log.debug("supporting_concept_mappings2 false");
      return false
    };
    if ((cm1.label.isEmpty)||(cm2.label.isEmpty)) {
      log.debug("supporting_concept_mappings3 false");
      return false
    };
    if ((cm1.label).equals(cm2.label)) {
      log.debug("supporting_concept_mappings4 true");
      return true
    };
    log.debug("supporting_concept_mappings5 false");
    return false;
  }

  def supporting_concept_mappingsWithXY(dcm1: List[ConceptMapping], dcm2: List[ConceptMapping], takeEquals: Boolean): Boolean = {
    val couples = for (
      xcm <- dcm1;
      ycm <- dcm2 if (takeEquals || (!takeEquals && xcm != ycm))
    ) yield (xcm, ycm)

    couples.find(c => supporting_concept_mappings(c._1, c._2)).isDefined
  }

  def internally_coherent(cm_list: List[ConceptMapping]): Boolean = {
    // returns true if there is any pair of relevant_distinguish
    // cms that support each other
    supporting_concept_mappingsWithXY(cm_list,cm_list,false)
  }

  def relevant_distinguishing_cms(c: CorrespondenceRep): List[ConceptMapping] = {
    val cms = ConceptMapping.conceptMappingsWithReps(c.concept_mapping_list)
    val result = cms.filter(cm => {
      val distinguishing = cm.distinguishing(log)
      log.debug(s"relevant_distinguishing_cms ${c.uuid} $cm relevant ${cm.relevant()} distinguishing ${distinguishing}")
      cm.relevant() && distinguishing
    })
    log.debug("result " + result)
    result
  }

  def convertDescriptionRepToInflatedDescriptionRep(d: DescriptionRep) = {
    val descriptionType = slipNodeRefs(d.descriptionType.id)
    val descriptor = d.descriptor.map(rep => slipNodeRefs(rep.id))
    InflatedDescriptionRep(
      d.uuid,
      descriptionType,
      descriptor
    )
  }

  def get_incompatible_bond(correspondence: CorrespondenceRep, incompatible_bond_base: Option[(BondRep,BondRep)]) : Option[BondRep] = {
    incompatible_bond_base match {
      case Some((bond1, bond2)) =>
        if (bond1.direction_category.isEmpty || bond2.direction_category.isEmpty) {
          None
        } else {
          val bond1DirCat = slipNodeRefs(bond1.direction_category.get.id)
          val bond2DirCat = slipNodeRefs(bond2.direction_category.get.id)

          val cm = ConceptMappingRep2(
            direction_category,
            direction_category,
            bond1DirCat,
            bond2DirCat,
            SlipnetFormulas.get_bond_category(bond1DirCat,bond2DirCat,identity)
          )
          val found = correspondence.concept_mapping_list.find(c => {
            val conceptMapping = ConceptMapping.conceptMappingRefs(c.uuid)
            incompatible_concept_mappings(conceptMapping.conceptMappingRep2(),cm)
          }).isDefined

          if (found) Some(bond2) else None
        }

      case None => None
    }
  }

  def get_incompatible_correspondences(correspondence: CorrespondenceRep, correspondenceReps: List[CorrespondenceRep]) = {
    // returns a list of all existing correspondences that are incompatible
    // with this proposed correspondence
    correspondenceReps.filter(c => {
      incompatible_correspondences(correspondence,c)
    })
  }

  def incompatible_correspondences(c1: CorrespondenceRep, c2: CorrespondenceRep) : Boolean = {
    if (c1.obj1==c2.obj1) return true
    if (c1.obj2==c2.obj2) return true
    c1.concept_mapping_list.find(c1cm => {
      c2.concept_mapping_list.find(c2cm => {
        val cm1 = ConceptMapping.conceptMappingRefs(c1cm.uuid)
        val cm2 = ConceptMapping.conceptMappingRefs(c2cm.uuid)

        incompatible_concept_mappings(
          cm1.conceptMappingRep2(),
          cm2.conceptMappingRep2()
        )
      }).isDefined
    }).isDefined
  }

  def set_conceptual_depths(cd: Double) = {
    for (ob <- slipNodes) {
      ob.conceptual_depth = cd
    }
  }


  def incompatible_concept_mappings(cm1: ConceptMappingRep2, cm2: ConceptMappingRep2): Boolean = {
    // Concept-mappings (a -> b) and (c -> d) are incompatible if a is
    // related to c or if b is related to d, and the a -> b relationship is
    // different from the c -> d relationship. E.g., rightmost -> leftmost
    // is incompatible with right -> right, since rightmost is linked
    // to right, but the relationships (opposite and identity) are different.
    // Notice that slipnet distances are not looked at, only slipnet links. This
    // should be changed eventually.
//    val cm1 = ConceptMapping.conceptMappingRefs(cm1Rep.uuid)
//    val cm2 = ConceptMapping.conceptMappingRefs(cm2Rep.uuid)

    if (!(SlipnetFormulas.related(cm1.descriptor1,cm2.descriptor1) ||
      SlipnetFormulas.related(cm1.descriptor2,cm2.descriptor2)))
      return false
    if (cm1.label.isEmpty || cm2.label.isEmpty) return false
    if (!(cm1.label==cm2.label)) return true
    false
  }


  def chooseSlipNodeWithTemperature(object_list: List[SlipNode], temperature: Double) = {
    val value_list = object_list.map(sn => {
      Formulas.temperatureAdjustedValue(sn.conceptual_depth, temperature)
    })
    val index = Utilities.valueProportionalRandomIndexInValueList(value_list)
    object_list(index)
  }

  def slippage_list_accumulation(acc: List[ConceptMapping], candidates: List[ConceptMapping]): List[ConceptMapping] = {
    candidates match {
      case Nil => acc
      case x :: xs => {
        val newAcc = if (!x.in_vector(acc)) x :: acc else acc
        slippage_list_accumulation(newAcc,xs)
      }
    }
  }


  def activateConceptMappingList(concept_mapping_list: List[ConceptMapping]): Unit = {
    // activate some descriptions
    for (cm <- concept_mapping_list) {
      cm.description_type1.setBuffer(100.0)
      cm.descriptor1.setBuffer(100.0)
      cm.description_type2.setBuffer(100.0)
      cm.descriptor2.setBuffer(100.0)
    }
  }

  def slipnetInfo() = SlipnetInfo(
    opposite,
    sameness,
    identity,
    letter,
    group,
    whole,
    slipnet_numbers.toList
  )


  def reset() = {
    System.out.println("Slipnet.Reset");

    number_of_updates=0;
    for (ob <- slipNodes){
      ob.setBuffer(0.0)
      ob.setActivation(0.0);
    }
    // clamp initially clamped slipnodes
    for (s <- initially_clamped_slipnodes){
      s.clamp=true
      s.setActivation(100.0)
    }

  }

  def update() = {
    log.debug("Slipnet.update()")
    // this procedure updates the slipnet
    number_of_updates += 1
    // unclamp initially clamped slipnodes if #of updates = 50

    if (number_of_updates==50) for (s <- initially_clamped_slipnodes) {
      s.clamp=false
      // GUIs.Redraw = true;
    }

    // for all nodes set old_activation to activation
    log.debug("for all nodes set old_activation to activation");

    for (ob <- slipNodes) {
      ob.old_activation=ob.activation
      log.debug("ob " + ob.id + " ob.buffer " + ob.buffer + " ob.activation " + ob.activation +  " ob.conceptual_depth " + ob.conceptual_depth);
      ob.setBuffer(ob.buffer - (ob.activation*((100.0-ob.conceptual_depth)/100.0)))

      if (ob==successor){
        //System.out.println("activation ="+ob.activation+" buffer="+ob.buffer);
        //System.out.println("number of nodes = "+slipnodes.size());
      }
    }


    // spreading activation
    // for all incomming links, if the activation of the sending node = 100
    // add the percentage of its activation to activation buffer
    if (!remove_spreading_activation) for (ob <- slipNodes) {
      //println(s"spreading activation ob ${ob.id()} size ${ob.outgoing_links.size}")
      for (sl <- ob.outgoing_links) {
        if (ob.activation == 100.0) {
          val slidegassos = sl.intrinsic_degree_of_association()
          log.debug(s"sl.to ${sl.to_node.id()} buffer ${sl.to_node.buffer} inc ${slidegassos}")

          sl.to_node.setBuffer(sl.to_node.buffer + slidegassos)
        }
      }
    }
    log.debug("Slipnet. update. slipnodes")
    // for all nodes add the activation activation_buffer
    // if activation>100 or clamp=true, activation=100
    for (ob <- slipNodes) {
      log.debug(s"clamp ${ob.clamp} ob ${ob.id()} ob.buffer ${ob.buffer} activation ${ob.activation}")

      if (!ob.clamp) {
        ob.setActivation( ob.activation + ob.buffer)
      }
      if (ob.activation>100.0) ob.setActivation(100.0)
      if (ob.activation<0.0) ob.setActivation(0.0)
      //println(s"ob ${ob.id()} clamp ${!ob.clamp} ob.buffer ${ob.buffer} activation ${ob.activation}")

    }


    // check for probabablistic jump to 100%
    log.debug("check for probabablistic jump to 100%");

    var act = 0.0
    log.debug("slipnodes.size()" + slipNodes.size)
    if (!remove_activation_jump) for (ob <- slipNodes) {
      act=ob.activation/100.0
      act=act*act*act

      log.debug("ob " + ob.id + " ob.activation " + ob.activation);

      if ((ob.activation>55.0) && ( Random.rnd(null) < act) && (!ob.clamp)) {
        ob.setActivation(100.0)
      }
      log.debug(s"ob ${ob.id()} activation ${ob.activation}")

    }



    // check for redraw; and reset buffer values to 0
    log.debug("check for redraw; and reset buffer values to 0");

    for (ob <- slipNodes) {
      ob.setBuffer(0.0)
      //val obActAsInt = (ob.activation/10.0).toInt
      //if ((ob.activation/10.0).toInt != (ob.old_activation/10.0).toInt ) {
        // GUI ob.Redraw=true;
        // GUI ob.child.Redraw = true;
      //}
      // From GraphicsObject (GUI)
      // ob.Values.addElement(new Double(ob.activation));

    }
  }

  def get_description_type_instance_links_to_node_info(description_type: SlipNode) = {
    val firstTos = description_type.instance_links.toList.filter(sl => sl.to_node == first).map(_.to_node.slipNodeRep())
    val lastTos = description_type.instance_links.toList.filter(sl => sl.to_node == last).map(_.to_node.slipNodeRep())
    val numbersTos = (0 to 4).map(y => {
      val yTos = description_type.instance_links.toList.filter(sl => sl.to_node == slipnet_numbers(y)).map(_.to_node.slipNodeRep())
      (y, TosInfo(slipnet_numbers(y).slipNodeRep(), yTos))
    }).toMap
    val middleTos = description_type.instance_links.toList.filter(sl => sl.to_node == middle).map(_.to_node.slipNodeRep())
    DescriptionTypeInstanceLinksToNodeInfo(
      TosInfo(slipnet_letters(0).slipNodeRep(), firstTos),
      TosInfo(slipnet_letters(25).slipNodeRep(), lastTos),
      numbersTos,
      middleTos)
  }


  def similar_has_property_links(s: SlipNode, t: Double) = s.has_property_links.toList.filter(sl => WorkspaceFormulas.flip_coin(
      Formulas.temperature_adjusted_probability(sl.degree_of_association()/100.0,t)
    )
  )

  def distinguishing_concept_mappings(concept_mapping_list: List[ConceptMapping]) = concept_mapping_list.filter(cm => {
    cm.distinguishing(log)
  })

  //def getDescriptor(workspaceObject: WorkspaceObject, node: SlipNode): Option[SlipNode] = ???
  //def getBondCategory(fromDescriptor: SlipNode, toDescriptor: SlipNode): Option[SlipNode] = ???



  def facetsOfAndPartOf(wo: WorkspaceObjectRep, facets: List[SlipNode]) = {
    val woDescriptions = wo.descriptions.toList.map(dt => slipNodeRefs(dt.descriptionType.id))
    woDescriptions.filter(dt => {
      facets.contains(dt)
    })
  }
}


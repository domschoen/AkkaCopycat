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
import models.Workspace.{GoWithBottomUpCorrespondenceScout2, InitializeWorkspaceStringsResponse}
import models.codelet.BottomUpCorrespondenceScout.ProposeAnyCorrespondenceSlipnetResponse
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport
import Description.DescriptionRep
import models.Bond.BondRep
import models.Group.GroupRep
import models.Letter.LetterSlipnetComplement
import models.SlipNode.{SlipNodeRep, SlipnetInfo}
import models.WorkspaceObject.WorkspaceObjectRep
import models.codelet.BottomUpDescriptionScout.SlipnetGoWithBottomUpDescriptionScoutResponse
import models.codelet.TopDownBondScoutCategory.SlipnetTopDownBondScoutCategory2Response
import models.codelet.TopDownBondScoutDirection.SlipnetTopDownBondScoutDirection2Response
import models.codelet.TopDownDescriptionScout.{SlipnetGoWithTopDownDescriptionScoutResponse, SlipnetGoWithTopDownDescriptionScoutResponse2}
import models.codelet.TopDownGroupScoutCategory.{SlipnetGoWithTopDownGroupScoutCategory2Response, SlipnetGoWithTopDownGroupScoutCategoryResponse}

import scala.collection.mutable.ListBuffer


object Slipnet {
  val r = scala.util.Random

  def props(): Props = Props(new Slipnet())

  object DirValue extends Enumeration {
    type DirValue = Value
    val Left, Right, None = Value
  }


  case class Run(initialString: String, modifiedString: String, targetString: String)
  case class InitializeSlipnet(coderack: ActorRef, workspace: ActorRef)
  case object UpdateEverything
  case class InitializeWorkspaceStrings(initialWos: List[LetterSlipnetComplement],
                                        modifiedWos: List[LetterSlipnetComplement],
                                        targetWos: List[LetterSlipnetComplement])

  case class BondFromTo(from: WorkspaceObjectRep, to: WorkspaceObjectRep)
  case class SlipnetTopDownBondScout(fromdtypes: List[SlipNodeRep], todtypes: List[SlipNodeRep])
  case class SlipnetTopDownBondScoutCategory2(bondCategory: String, from_descriptor: SlipNodeRep, to_descriptor: SlipNodeRep)
  case class SlipnetTopDownBondScoutDirection2(bondCategory: String, from_descriptor: SlipNodeRep, to_descriptor: SlipNodeRep)

  case class BondFromTo2(
                          from: WorkspaceObjectRep,
                          to: WorkspaceObjectRep,
                          fromDescriptor: SlipNodeRep,
                          toDescriptor: SlipNodeRep)

  case class ProposeAnyCorrespondence(
                                       obj1 :WorkspaceObjectRep,
                                       obj2: WorkspaceObjectRep,
                                       temperature: Double
                                     )
  /*case class ProposeAnyCorrespondence2(
                                       obj1 :WorkspaceStructureRep,
                                       obj2: WorkspaceStructureRep,
                                       //temperature: Double,
                                       codelet: ActorRef
                                      )*/
  case class CompleteProposeGroup(grCategoryID: String, dirCategoryID: Option[String])
  case class CompleteProposeGroupResponse(urgency: Double)

  case class SlipnetGoWithTopDownDescriptionScout(chosen_object: WorkspaceObjectRep, descriptionTypeID: String)
  case class GoWithTopDownDescriptionScoutResponse2(chosen_property: SlipNodeRep)
  case class GoWithTopDownBondScout2Response(bond_facet: SlipNodeRep, from_descriptor: SlipNodeRep, to_descriptor: SlipNodeRep)

  case class SetSlipNodeBufferValue(slipNodeID: String, bufferValue: Double)
  case class SlipnetGoWithBottomUpDescriptionScout(slipNodeRep: SlipNodeRep, temperature: Double)

  case class SlipnetTopDownBondScoutResponse(fromdtypes: List[SlipNodeRep], todtypes: List[SlipNodeRep])

  case class TosInfo(slipNodeRef: SlipNodeRep, tos: List[SlipNodeRep])
  case class DescriptionTypeInstanceLinksToNodeInfo(
                                                     firstTos: TosInfo,
                                                     lastTos: TosInfo,
                                                     numbersTos: Map[Int, TosInfo],
                                                     middleTos: List[SlipNodeRep]
                                                   )
  case class SlipnetGoWithTopDownGroupScoutCategory(groupID: String, temperature: Double)
  case class SlipnetGoWithTopDownGroupScoutCategory2(dir: DirValue.DirValue)
  case class SlipnetGoWithTopDownGroupScoutCategory3(bond_category: SlipNodeRep, fromOBRep: WorkspaceObjectRep)

  case class InflatedDescriptionRep(uuid :String, descriptionTypeSlipNode: SlipNode, descriptorSlipNode: Option[SlipNode]) {
    //def descriptionType(mapping: Map[String, SlipNode]) = mapping()
  }



  object RelationType {
    val Sameness = "Sameness"
    val Successor = "Successor"
    val Predecessor = "Predecessor"
  }
  val time_step_length = 15

}


class Slipnet extends Actor with ActorLogging with InjectedActorSupport {

  import Coderack.{ Run, ProposeCorrespondence}
  import Slipnet._
  import models.codelet.Codelet.Finished
  import models.codelet.BottomUpBondScout.{ BondFromToSlipnetResponse, BondFromTo2Response }

  var woAppActor: Option[ActorRef] = None
  var coderack: ActorRef = null
  var workspace: ActorRef = null


  var remove_spreading_activation = false
  var remove_activation_jump = false

  var bondFacets = List.empty[SlipNode]
  var slipNodeRefs = Map.empty[String, SlipNode]
  var slipNodes = ListBuffer.empty[SlipNode]

  //var sameness = Option.empty[SlipNode]
  //var opposite = Option.empty[SlipNode]
  //var group_category = Option.empty[SlipNode]
  //var direction_category = Option.empty[SlipNode]

  var chars = (0 to 25).map(i => (i + 65).toChar.toString)
  var slipnet_letters = chars.map(c => addBasicSlipNode(0, 0, 10.0, c, c)).to[ListBuffer]

  var numbers = (0 to 4).map(i => (i + 49).toChar.toString)
  var slipnet_numbers: ListBuffer[SlipNode] = numbers.map(c => addBasicSlipNode(0, 0, 30.0, c, c)).to[ListBuffer]

  // moved to singleton
  // val time_step_length = 15
  var number_of_updates = 0

  // string positions
  val leftmost = add_slipnode(17, 18, 40.0, "leftmost", "lm")
  val rightmost = add_slipnode(27, 18, 40.0, "rightmost", "rm")
  val middle = add_slipnode(27, 26, 40.0, "middle", "md")
  val single = add_slipnode(33, 26, 40.0, "single", "sl")
  val whole = add_slipnode(30, 26, 40.0, "whole", "wh")

  // alphabetic positions
  val first = add_slipnode(19, 15, 60.0, "first", "fs")
  val last = add_slipnode(25, 15, 60.0, "last", "ls")

  // directions
  val left = add_slipnode(17, 22, 40.0, "left", "lf")
  left.codelets += "top-down-bond-scout--direction"
  left.codelets += "top-down-group-scout--direction"
  val right = add_slipnode(27, 22, 40.0, "right", "rt")
  right.codelets += "top-down-bond-scout--direction"
  right.codelets += "top-down-group-scout--direction"

  // bond types
  val predecessor = add_slipnode(14, 38, 50.0, "predecessor", "pd") //,60.0)
  predecessor.codelets += "top-down-bond-scout--category"

  val successor = add_slipnode(14, 33, 50.0, "successor", "sc") //,60.0)
  successor.codelets += "top-down-bond-scout--category"
  val sameness = add_slipnode(10, 29, 80.0, "sameness", "sm") //,0.0)
  sameness.codelets += "top-down-bond-scout--category"

  // group types
  val predgrp = add_slipnode(20, 38, 50.0, "predecessor group", "pg")
  predgrp.codelets += "top-down-group-scout--category"
  val succgrp = add_slipnode(20, 33, 50.0, "successor group", "sg")
  succgrp.codelets += "top-down-group-scout--category"
  val samegrp = add_slipnode(10, 25, 80.0, "sameness group", "smg")
  samegrp.codelets += "top-down-group-scout--category"

  // other relations
  val identity = add_slipnode(2, 30, 90.0, "identity", "id") //,0.0)
  val opposite = add_slipnode(6, 30, 90.0, "opposite", "op") //,80.0)

  // objects
  val letter = add_slipnode(2, 38, 20.0, "letter", "l")
  val group = add_slipnode(6, 38, 80.0, "group", "g")

  // categories
  val letter_category = add_slipnode(22, 9, 30.0, "letter category", "lc")
  val string_position_category = add_slipnode(30, 21, 70.0, "string position", "spc")
  string_position_category.codelets += "top-down-description-scout"
  val alphabetic_position_category = add_slipnode(22, 12, 80.0, "alphabetic position", "apc")
  alphabetic_position_category.codelets += "top-down-description-scout"
  val direction_category = add_slipnode(22, 25, 70.0, "direction category", "dc")
  val bond_category = add_slipnode(10, 33, 80.0, "bond category", "bc")
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


  //***************************************************************
  //   initialise links between nodes

  // **************   successor and predecessor links
  var sliplinks = ListBuffer.empty[SlipnetLink]
  // letters
  linkSuccessiveSlipNodes(slipnet_letters)
  linkSuccessiveSlipNodes(slipnet_numbers)
  // ************** letter category links
  linkWith(slipnet_letters, letter_category, 97.0)
  for (i <- 0 to slipnet_letters.size - 1) {
    add_category_link(slipnet_letters(i), letter_category, letter_category.conceptual_depth - slipnet_letters(i).conceptual_depth);
    add_instance_link(letter_category, slipnet_letters(i), 97.0);
  }
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

  def linkSuccessiveSlipNodes(nodes: ListBuffer[SlipNode]) = for (i <- 0 to nodes.size - 2) {
    add_nonslip_link(nodes(i), nodes(i + 1), successor);
    add_nonslip_link(nodes(i + 1), nodes(i), predecessor);
  }

  def linkWith(nodes: ListBuffer[SlipNode], withSplipNode: SlipNode, len: Double) = for (i <- 0 to nodes.size - 1) {
    add_category_link(nodes(i), withSplipNode, withSplipNode.conceptual_depth - nodes(i).conceptual_depth);
    add_instance_link(withSplipNode, nodes(i), len);
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
    val slipNode = new SlipNode(x, y, cd, pn, sn)
    slipNodeRefs += (slipNode.id() -> slipNode)
    slipNodes += slipNode
    slipNode
  }

  def add_slipnode(x: Int, y: Int, cd: Double, pn: String, sn: String): SlipNode = {
    addBasicSlipNode(x * 25, y * 25, cd, pn, sn)
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
  def groupFlipped_version(obj: WorkspaceObjectRep): Option[WorkspaceObjectRep] = {
    val groupRep = obj.groupRep.get
    val bond_list = groupRep.bond_list

    // returns a flipped version of this group
    val new_bond_list = bond_list.map(b => bondFlipped_version(b))
    val group_category = slipNodeRefs(groupRep.groupCategorySlipNodeID)
    if (groupRep.directionCategorySlipNodeID.isDefined) {
      val direction_category = slipNodeRefs(groupRep.directionCategorySlipNodeID.get)
      val relatedGroup_category = SlipnetFormulas.get_related_node(group_category, opposite, identity).get
      val relatedDirection_categoryOpt = SlipnetFormulas.get_related_node(direction_category, opposite, identity)
      val relatedDirection_categoryID = relatedDirection_categoryOpt match {
        case Some(relatedDirection_category) => Some(relatedDirection_category.id())
        case None => None
      }

      val flippedGroup = GroupRep(
        relatedGroup_category.id(),
        relatedDirection_categoryID,
        groupRep.bondFacetSlipNodeID, new_bond_list)

      Some(obj.copy(groupRep = Some(flippedGroup)))

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
      bond.from_obj_descriptorSlipNodeID) // swapped with above

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
      WorkspaceObjectRep(l.uuid, descriptions.toList, List(), false, None, None, None)
    })


  }


  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString) => {
      log.debug(s"Slipnet | Run with initial $initialString, modified: $modifiedString and target: $targetString")
    }
    case InitializeSlipnet(cr, ws) =>
      coderack = cr
      workspace = ws

    case UpdateEverything =>
      update()

    case InitializeWorkspaceStrings(initialWos, modifiedWos, targetWos) =>
      val initialDescriptions = letterDescriptionReps(initialWos)
      val modifiedDescriptions = letterDescriptionReps(modifiedWos)
      val targetDescriptions = letterDescriptionReps(targetWos)
      workspace ! InitializeWorkspaceStringsResponse(initialDescriptions, modifiedDescriptions, targetDescriptions)


    case SetSlipNodeBufferValue(slipNodeID: String, bufferValue: Double) =>
      slipNodeRefs(slipNodeID).buffer = bufferValue

    // bottom-up-bond-scout codelet.java.255
    case BondFromTo(from, to) =>

      // choose_bond_facet, workspace_formulas.java.191
      val fromDescriptionFacets: List[SlipNode] = facetsOfAndPartOf(from, bondFacets)
      val toDescriptionFacets: List[SlipNode] = facetsOfAndPartOf(to, fromDescriptionFacets)

      val fromFacetSlipNodeReps = fromDescriptionFacets.map(_.slipNodeRep())
      val toFacetSlipNodeReps = fromDescriptionFacets.map(_.slipNodeRep())

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
          fromob_facets.map(sn => sn.slipNodeRep()),
          local_bond_facets.map(sn => sn.slipNodeRep()),
        )
      }
    case SlipnetTopDownBondScoutCategory2(bondCategory, fromDescriptor, toDescriptor) =>
      val from_descriptor = slipNodeRefs(fromDescriptor.id)
      val to_descriptor = slipNodeRefs(toDescriptor.id)


      val bc1 = SlipnetFormulas.get_bond_category(from_descriptor,to_descriptor, identity)
      val bc2 = SlipnetFormulas.get_bond_category(to_descriptor,from_descriptor, identity)

      // Added test  compare to JavaCopycat
      if (bc1.isEmpty || bc2.isEmpty) {
        print("Oups Bond category empty: Fizzle")
        sender() ! Finished
      } else {
        val b1r = bc1.get
        val b2r = bc2.get

        val b1 = if (b1r == identity) sameness else b1r
        val b2 = if (b1r == identity) sameness else b2r

        val bond_category = slipNodeRefs(bondCategory)

        if ((bond_category!=b1)&&(bond_category!=b2)){
          print("no suitable link: Fizzle!");
          sender() ! Finished
        } else {
          val isFromTo = bond_category==b1
          val urgency = bond_category.bond_degree_of_association();

          bond_facet.buffer=100.0;
          from_descriptor.buffer=100.0;
          to_descriptor.buffer=100.0;

          sender() ! SlipnetTopDownBondScoutCategory2Response(isFromTo,urgency, bond_category.slipNodeRep(),
            left.slipNodeRep(),
            right.slipNodeRep()
          )
        }
      }
    case SlipnetTopDownBondScoutDirection2(bondCategory, fromDescriptor, toDescriptor) =>
      val from_descriptor = slipNodeRefs(fromDescriptor.id)
      val to_descriptor = slipNodeRefs(toDescriptor.id)


      val bond_categoryOpt = SlipnetFormulas.get_bond_category(from_descriptor,to_descriptor, identity)

      // Added test  compare to JavaCopycat
      if (bond_categoryOpt.isEmpty) {
        print("Oups Bond category empty: Fizzle")
        sender() ! Finished
      } else {
        val bc = bond_categoryOpt.get

        val bond_category = if (bc == identity) sameness else bc


          val urgency = bond_category.bond_degree_of_association();

          bond_facet.buffer=100.0;
          from_descriptor.buffer=100.0;
          to_descriptor.buffer=100.0;

          sender() ! SlipnetTopDownBondScoutDirection2Response(urgency, bond_category.slipNodeRep(),
            left.slipNodeRep(),
            right.slipNodeRep()
          )
      }




    // bottom-up-bond-scout codelet.java.267
    case BondFromTo2(from,to,fromDescriptor,toDescriptor) =>
      val from_descriptor = slipNodeRefs(fromDescriptor.id)
      val to_descriptor = slipNodeRefs(toDescriptor.id)
      val bondCategoryOpt = SlipnetFormulas.get_bond_category(from_descriptor, to_descriptor, identity)
      bondCategoryOpt match {
        case None =>
          log.debug(" no suitable link - fizzle")
          sender() ! Finished
        case Some(bondCategory) =>
          val adaptedBondCategory = if (bondCategory==identity) sameness else bondCategory
          // there is a possible bond, so propose it
          log.info(s"proposing ${adaptedBondCategory.name} bond ")
          // coderack.propose_bond(fromob,toob,bond_category,bond_facet,from_descriptor, to_descriptor,this);
          // coderack.java.274
          bond_facet.buffer=100.0;
          from_descriptor.buffer=100.0;
          to_descriptor.buffer=100.0;

          sender() ! BondFromTo2Response(
            adaptedBondCategory.slipNodeRep(),
            adaptedBondCategory.bond_degree_of_association(),
            left.slipNodeRep(),
            right.slipNodeRep()
          )
      }

    // codelet.java.1233
    case ProposeAnyCorrespondence(obj1, obj2, temperature) =>
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
        obj2Relevant_descriptions, obj2Relevant_descriptions, slipnetInfo)

      // check the slippability of concept mappings
      val cm_possible = concept_mapping_list.find(cm => {
        val slip_prob = WorkspaceFormulas.temperature_adjusted_probability(cm.slipability() / 100.0, temperature)
        WorkspaceFormulas.flip_coin(slip_prob)
      })

      if (concept_mapping_list.isEmpty) {
        // no possible mappings
        print("no possible mappings exist: fizzle")
        sender() ! Finished

      } else {
        if (cm_possible.isEmpty) {
          //cannot make necessary slippages
          print("cannot make appropriate slippage: fizzle");
          sender() ! Finished

        } else {
          //find out if any are distinguishing
          val distinguishing_mappings = concept_mapping_list.filter(cm => cm.distinguishing())

          if (distinguishing_mappings.isEmpty) {
            // no distinguishing mappings
            print("no distinguishing mappings found: fizzle");
            sender() ! Finished

          } else {
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

            val newObj2Opt = if (flip_obj2) groupFlipped_version(obj2) else Some(obj2)
            newObj2Opt match {
              case None =>
                print("no groupFlipped_version found: fizzle");
                sender() ! Finished

              case Some(newObj2) =>
                val newconcept_mapping_list = if (flip_obj2) {
                  val newObj2Descriptions = newObj2.descriptions.map(inflatedDescriptionRep)
                  val newObj2Relevant_descriptions = relevant_descriptions(newObj2Descriptions)

                  ConceptMapping.get_concept_mapping_list(
                    obj1, newObj2,
                    obj2Relevant_descriptions, newObj2Relevant_descriptions, slipnetInfo);
                } else concept_mapping_list

                println("Proposing correspondence with concept mappings:");
                for(cm <- newconcept_mapping_list) {
                  println(cm.toString())
                }

                // Coderack.java.323 (propose_correspondence)

                // activate some descriptions
                for (cm <- newconcept_mapping_list) {
                  cm.description_type1.buffer=100.0
                  cm.descriptor1.buffer=100.0
                  cm.description_type2.buffer=100.0
                  cm.descriptor2.buffer=100.0
                }

                val dcm = distinguishing_concept_mappings(newconcept_mapping_list)
                val totalStrength = dcm.map(_.strength()).sum

                val conceptMappingReps = concept_mapping_list.map(cm => cm.conceptMappingRep())
                sender() ! ProposeAnyCorrespondenceSlipnetResponse(
                  obj1,
                  newObj2,
                  conceptMappingReps,
                  flip_obj2,
                  dcm.size,
                  totalStrength
                )

            }

          }
        }
      }

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
            sender() ! SlipnetGoWithBottomUpDescriptionScoutResponse(chosen_propertyRep, cat.slipNodeRep())

          case None =>
            sender() ! Finished
        }
      }

    case SlipnetGoWithTopDownDescriptionScout(chosen_object, descriptonTypeID) =>
      val description_type = slipNodeRefs(descriptonTypeID)
      val info = get_description_type_instance_links_to_node_info(description_type);
      sender() ! SlipnetGoWithTopDownDescriptionScoutResponse(info)

    case GoWithTopDownDescriptionScoutResponse2(cp) =>
      val chosen_property = slipNodeRefs(cp.id)
      chosen_property.category() match {
        case Some(cat) =>
          sender() ! SlipnetGoWithTopDownDescriptionScoutResponse2(cat.slipNodeRep())

        case None =>
          sender() ! Finished
      }

    case SlipnetGoWithTopDownGroupScoutCategory(groupID, temperature) =>
      val group_cat = slipNodeRefs(groupID)
      val bond_categoryOpt = SlipnetFormulas.get_related_node(group_cat, bond_category, identity)
      bond_categoryOpt match {
        case Some(bond_category) =>
          sender() ! SlipnetGoWithTopDownGroupScoutCategoryResponse(bond_category.slipNodeRep())
        case None =>
          log.debug("<c> no bond-category found")
          sender() ! Finished

      }
    case SlipnetGoWithTopDownGroupScoutCategory2(dir) =>
      val direction = dir match {
        case DirValue.Right =>
          DirValue.Left
        case DirValue.Left =>
          DirValue.Right
        case DirValue.None =>
          val v = List(left.activation, right.activation)
          if (Utilities.valueProportionalRandomIndexInValueList(v) == 0) DirValue.Left else DirValue.Right
      }
      //print("trying from "+fromob+" "+bond_category.pname+" checking to "+direction.pname+" first");
      sender() ! SlipnetGoWithTopDownGroupScoutCategory2Response(direction, length.activation)

    case CompleteProposeGroup(grCategoryID: String, dirCategoryIDOpt: Option[String]) =>
      val grCategory = slipNodeRefs(grCategoryID)
      val bond_categoryOpt = SlipnetFormulas.get_related_node(grCategory,bond_category, identity)
      // match added compare to JavaCopycat
      bond_categoryOpt match {
        case Some(bond_category) =>
          bond_category.buffer=100.0;
          dirCategoryIDOpt match {
            case Some(dirCategoryID) =>
              val dirCategory = slipNodeRefs(dirCategoryID)
              dirCategory.buffer=100.0
            case None =>
          }
          val urgency = bond_category.bond_degree_of_association()

          sender() ! CompleteProposeGroupResponse(urgency)

        case None =>
          log.debug("<c> no bond-category found")
          sender() ! Finished
      }
    case SlipnetGoWithTopDownGroupScoutCategory3(bond_category, fromob) =>

  }

  def update() = {
    // this procedure updates the slipnet
    number_of_updates += 1
    // unclamp initially clamped slipnodes if #of updates = 50

    if (number_of_updates==50){
      for (s <- initially_clamped_slipnodes) {
        s.clamp=false;
        // GUIs.Redraw = true;
      }
    }

    // for all nodes set old_activation to activation
    for (ob <- slipNodes) {
      ob.old_activation=ob.activation;
      ob.buffer-=ob.activation*((100.0-ob.conceptual_depth)/100.0);
      if (ob==successor){
        //System.out.println("activation ="+ob.activation+" buffer="+ob.buffer);
        //System.out.println("number of nodes = "+slipnodes.size());
      }
    }


    // spreading activation
    // for all incomming links, if the activation of the sending node = 100
    // add the percentage of its activation to activation buffer
    if (!remove_spreading_activation){
      for (ob <- slipNodes) {
        for (sl <- ob.outgoing_links) {
          if (ob.activation == 100.0){
            (sl.to_node).buffer += sl.intrinsic_degree_of_association()
          }
        }
      }
    }
    // for all nodes add the activation activation_buffer
    // if activation>100 or clamp=true, activation=100
    for (ob <- slipNodes) {
      if (!(ob.clamp)) ob.activation+=ob.buffer;
      if (ob.activation>100.0) ob.activation=100.0;
      if (ob.activation<0.0) ob.activation = 0.0;
    }


    // check for probabablistic jump to 100%
    var act : Double = 0.0
    if (!remove_activation_jump){
      for (ob <- slipNodes) {
        act=ob.activation/100.0
        act=act*act*act

        if ((ob.activation>55.0)&& ( Slipnet.r.nextDouble() < act) &&
          (!(ob.clamp))) {
          ob.activation=100.0
        };
      }
    }


    // check for redraw; and reset buffer values to 0
/*  GUI
    for (ob <- slipNodes) {
      ob.buffer = 0.0;
      val obActAsInt = (ob.activation/10.0).toInt
      if ((ob.activation/10.0).toInt != (ob.old_activation/10.0).toInt ) {
        // GUI ob.Redraw=true;
        // GUI ob.child.Redraw = true;
      }
      // From GraphicsObject (GUI)
      // ob.Values.addElement(new Double(ob.activation));

    }*/
  }

  def get_description_type_instance_links_to_node_info(description_type: SlipNode): DescriptionTypeInstanceLinksToNodeInfo = {
    val firstTos = description_type.instance_links.toList.filter(sl => sl.to_node == first).map(_.to_node.slipNodeRep())
    val lastTos = description_type.instance_links.toList.filter(sl => sl.to_node == last).map(_.to_node.slipNodeRep())
    val numbersTos = (0 to 4).map(y => {
      val yTos = description_type.instance_links.toList.filter(sl => (sl.to_node == slipnet_numbers(y))).map(_.to_node.slipNodeRep())
      (y, TosInfo(slipnet_numbers(y).slipNodeRep(), yTos))
    }).toMap
    val middleTos = description_type.instance_links.toList.filter(sl => sl.to_node == middle).map(_.to_node.slipNodeRep())
    DescriptionTypeInstanceLinksToNodeInfo(
      TosInfo(slipnet_letters(0).slipNodeRep(), firstTos),
      TosInfo(slipnet_letters(25).slipNodeRep(), lastTos),
      numbersTos,
      middleTos)
  }


  def similar_has_property_links(s: SlipNode, t: Double): List[SlipnetLink] ={
    s.has_property_links.toList.filter(sl => WorkspaceFormulas.flip_coin(
        Formulas.temperature_adjusted_probability(sl.degree_of_association()/100.0,t)
      )
    )
  }

  def distinguishing_concept_mappings(concept_mapping_list: List[ConceptMapping]): List[ConceptMapping] = {
    concept_mapping_list.filter(cm => cm.distinguishing())
  }

  //def getDescriptor(workspaceObject: WorkspaceObject, node: SlipNode): Option[SlipNode] = ???
  //def getBondCategory(fromDescriptor: SlipNode, toDescriptor: SlipNode): Option[SlipNode] = ???



  def facetsOfAndPartOf(wo: WorkspaceObjectRep, facets: List[SlipNode]): List[SlipNode] = {
    val woDescriptions = wo.descriptions.toList.map(dt => slipNodeRefs(dt.descriptionType.id))
    woDescriptions.filter(dt => {
      facets.contains(dt)
    })
  }
}


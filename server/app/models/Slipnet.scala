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
import models.Letter.LetterSlipnetComplement
import models.SlipNode.{SlipNodeRep, SlipnetInfo}

import scala.collection.mutable.ListBuffer

class SlipnetLink(var from_node: SlipNode, var to_node: SlipNode, var label: SlipNode, var fixed_length: Double) {

  var slip_link = false;
  def this(fr: SlipNode, to: SlipNode, lab: SlipNode) = {
    this(fr, to, lab, 0.0)
  }
  def this(fr: SlipNode, to: SlipNode, len: Double) = {
    this(fr, to, null, len)
    fr.outgoing_links += this
  }
  def degree_of_association(): Double = {
    if ((fixed_length>0.0)||(label==null)) {
      100.0-fixed_length
    } else {
      label.degree_of_association()
    }
  }


}

object Slipnet {

  def props(): Props = Props(new Slipnet())

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case class InitializeSlipnet(coderack: ActorRef, workspace: ActorRef)
  case class InitializeWorkspaceStrings(initialWos: List[LetterSlipnetComplement],
                                        modifiedWos: List[LetterSlipnetComplement],
                                        targetWos: List[LetterSlipnetComplement])

  case class BondFromTo(from: WorkspaceStructureRep, to: WorkspaceStructureRep)
  case class BondFromTo2(
                          from: WorkspaceStructureRep,
                          to: WorkspaceStructureRep,
                          fromDescriptor: SlipNodeRep,
                          toDescriptor: SlipNodeRep)

  case class ProposeAnyCorrespondence(
                                       obj1 :WorkspaceStructureRep,
                                       obj2: WorkspaceStructureRep,
                                       temperature: Double
                                     )
  /*case class ProposeAnyCorrespondence2(
                                       obj1 :WorkspaceStructureRep,
                                       obj2: WorkspaceStructureRep,
                                       //temperature: Double,
                                       codelet: ActorRef
                                      )*/
  case class SetSlipNodeBufferValue(slipNodeID: String, bufferValue: Double)

  case class WorkspaceStructureRep(
                                    uuid :String,
                                    descriptions: List[DescriptionRep],
                                    letterOrGroupCompanionReps: List[WorkspaceStructureRep],
                                    spans_string: Boolean,
                                    groupRep: Option[GroupRep]
                                  )
  case class InflatedDescriptionRep(uuid :String, descriptionTypeSlipNode: SlipNode, descriptorSlipNode: Option[SlipNode]) {
    //def descriptionType(mapping: Map[String, SlipNode]) = mapping()
  }

  case class GroupRep(              groupCategorySlipNodeID: String,
                                    directionCategorySlipNodeID: String,
                                    bondFacetSlipNodeID: String,
                                    bond_list: List[BondRep]
                     )


  val time_step_length = 15

  object RelationType {
    val Sameness = "Sameness"
    val Successor = "Successor"
    val Predecessor = "Predecessor"
  }
}


class Slipnet extends Actor with ActorLogging with InjectedActorSupport {

  import Coderack.{ Run, ProposeCorrespondence}
  import Slipnet._
  import models.codelet.Codelet.Finished
  import models.codelet.BottomUpBondScout.{ BondFromToSlipnetResponse, BondFromTo2Response }

  var woAppActor: Option[ActorRef] = None
  var coderack: ActorRef = null
  var workspace: ActorRef = null
  var bondFacets = List.empty[SlipNode]
  var slipNodeRefs = Map.empty[String, SlipNode]

  def slipNodes() = slipNodeRefs.values.toList

  //var sameness = Option.empty[SlipNode]
  //var opposite = Option.empty[SlipNode]
  //var group_category = Option.empty[SlipNode]
  //var direction_category = Option.empty[SlipNode]

  var chars = (0 to 25).map(i => (i + 65).toChar.toString)
  var slipnet_letters = chars.map(c => addBasicSlipNode(0, 0, 10.0, c, c)).to[ListBuffer]

  var numbers = (0 to 4).map(i => (i + 49).toChar.toString)
  var slipnet_numbers: ListBuffer[SlipNode] = numbers.map(c => addBasicSlipNode(0, 0, 30.0, c, c)).to[ListBuffer]


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
  def groupFlipped_version(obj: WorkspaceStructureRep): WorkspaceStructureRep = {
    val groupRep = obj.groupRep.get
    val bond_list = groupRep.bond_list

    // returns a flipped version of this group
    val new_bond_list = bond_list.map(b => bondFlipped_version(b))
    val group_category = slipNodeRefs(groupRep.groupCategorySlipNodeID)
    val direction_category = slipNodeRefs(groupRep.directionCategorySlipNodeID)

    val relatedGroup_category = SlipnetFormulas.get_related_node(group_category, opposite, identity).get
    val relatedDirection_category = SlipnetFormulas.get_related_node(direction_category, opposite, identity).get

    val flippedGroup = GroupRep(
      relatedGroup_category.id(),
      relatedDirection_category.id(),
      groupRep.bondFacetSlipNodeID, new_bond_list)

    obj.copy(groupRep = Some(flippedGroup))
  }


  def bondFlipped_version(bond: BondRep): BondRep = {
    val bond_category = slipNodeRefs(bond.bondCategorySlipNodeID)
    val related = SlipnetFormulas.get_related_node(bond_category,opposite, identity).get



    //returns the flipped version of this bond
    BondRep(bond.to_obj,bond.from_obj,
      related.id(),
      bond.bondFacetSlipNodeID,
      bond.to_obj_descriptorSlipNodeID, // swapped with below
      bond.from_obj_descriptorSlipNodeID); // swapped with above

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
      WorkspaceStructureRep(l.uuid, descriptions.toList, List(), false, None)
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

            val newObj2 = if (flip_obj2) groupFlipped_version(obj2) else obj2

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

  }

  def distinguishing_concept_mappings(concept_mapping_list: List[ConceptMapping]): List[ConceptMapping] = {
    concept_mapping_list.filter(cm => cm.distinguishing())
  }

  //def getDescriptor(workspaceObject: WorkspaceObject, node: SlipNode): Option[SlipNode] = ???
  //def getBondCategory(fromDescriptor: SlipNode, toDescriptor: SlipNode): Option[SlipNode] = ???





  def facetsOfAndPartOf(wo: WorkspaceStructureRep, facets: List[SlipNode]): List[SlipNode] = {
    val woDescriptions = wo.descriptions.toList.map(dt => slipNodeRefs(dt.descriptionType.id))
    woDescriptions.filter(dt => {
      facets.contains(dt)
    })
  }
}


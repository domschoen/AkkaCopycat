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



class SlipNode(x: Int, y: Int, val conceptual_depth: Double, val name: String, shortName: String) {
  var codelets = ListBuffer.empty[String]
  var lateral_nonslip_links = ListBuffer.empty[SlipnetLink]
  var lateral_slip_links = ListBuffer.empty[SlipnetLink]
  var incoming_links = ListBuffer.empty[SlipnetLink]
  val outgoing_links = ListBuffer.empty[SlipnetLink]
  val instance_links = ListBuffer.empty[SlipnetLink]
  val category_links = ListBuffer.empty[SlipnetLink]
  val has_property_links = ListBuffer.empty[SlipnetLink]

  var clamp = false
}
class SlipnetLink(from_node: SlipNode, to_node: SlipNode, label: SlipNode, var fixed_length: Double) {
  var slip_link = false;

  def this(fr: SlipNode, to: SlipNode, lab: SlipNode) = {
    this(fr, to, lab, 0.0)
  }
  def this(fr: SlipNode, to: SlipNode, len: Double) = {
    this(fr, to, null, len)
    fr.outgoing_links += this
  }

}

object Slipnet {
  def props(): Props = Props(new Slipnet())

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case class SetCoderack(coderack: ActorRef)
  case class BondFromTo(from: WorkspaceObject, to: WorkspaceObject)

  val time_step_length = 15
}


class Slipnet extends Actor with ActorLogging with InjectedActorSupport {
  import Coderack.Run
  import Slipnet._
  var woAppActor: Option[ActorRef] = None
  var coderack : ActorRef = null
  var bondFacets = List.empty[SlipNode]

  //var sameness = Option.empty[SlipNode]
  //var opposite = Option.empty[SlipNode]
  //var group_category = Option.empty[SlipNode]
  //var direction_category = Option.empty[SlipNode]

  var chars = (0 to 25).map(i => (i+65).toChar.toString)
  var slipnet_letters = chars.map(c => new SlipNode(0,0, 10.0, c, c)).to[ListBuffer]

  var numbers = (0 to 4).map(i => (i+49).toChar.toString)
  var slipnet_numbers = numbers.map(c => new SlipNode(0,0, 30.0, c, c)).to[ListBuffer]

  var slipNodes = slipnet_letters ++ slipnet_numbers



  // string positions
  val leftmost = add_slipnode(17,18,40.0,"leftmost","lm")
  val rightmost = add_slipnode(27,18,40.0,"rightmost","rm")
  val middle = add_slipnode(27,26,40.0,"middle","md")
  val single = add_slipnode(33,26,40.0,"single","sl")
  val whole = add_slipnode(30,26,40.0,"whole","wh")

  // alphabetic positions
  val first = add_slipnode(19,15,60.0,"first","fs")
  val last = add_slipnode(25,15,60.0,"last","ls")

  // directions
  val left = add_slipnode(17,22,40.0,"left","lf")
  left.codelets += "top-down-bond-scout--direction"
  left.codelets += "top-down-group-scout--direction"
  val right = add_slipnode(27,22,40.0,"right","rt")
  right.codelets += "top-down-bond-scout--direction"
  right.codelets += "top-down-group-scout--direction"

  // bond types
  val predecessor = add_slipnode(14,38,50.0,"predecessor","pd") //,60.0)
  predecessor.codelets += "top-down-bond-scout--category"

  val successor = add_slipnode(14,33,50.0,"successor","sc") //,60.0)
  successor.codelets += "top-down-bond-scout--category"
  val sameness = add_slipnode(10,29,80.0,"sameness","sm") //,0.0)
  sameness.codelets += "top-down-bond-scout--category"

  // group types
  val predgrp = add_slipnode(20,38,50.0,"predecessor group","pg")
  predgrp.codelets += "top-down-group-scout--category"
  val succgrp = add_slipnode(20,33,50.0,"successor group","sg")
  succgrp.codelets += "top-down-group-scout--category"
  val samegrp = add_slipnode(10,25,80.0,"sameness group","smg")
  samegrp.codelets += "top-down-group-scout--category"

  // other relations
  val identity = add_slipnode(2,30,90.0,"identity","id") //,0.0)
  val opposite = add_slipnode(6,30,90.0,"opposite","op") //,80.0)

  // objects
  val letter = add_slipnode(2,38,20.0,"letter","l")
  val group = add_slipnode(6,38,80.0,"group","g")

  // categories
  val letter_category = add_slipnode(22,9,30.0,"letter category","lc")
  val string_position_category = add_slipnode(30,21,70.0,"string position","spc")
  string_position_category.codelets += "top-down-description-scout"
  val alphabetic_position_category = add_slipnode(22,12,80.0,"alphabetic position","apc")
  alphabetic_position_category.codelets += "top-down-description-scout"
  val direction_category = add_slipnode(22,25,70.0,"direction category","dc")
  val bond_category = add_slipnode(10,33,80.0,"bond category","bc")
  val group_category = add_slipnode(17,29,80.0,"group category","gpc")
  val length = add_slipnode(36,32,60.0,"length","len")
  val object_category = add_slipnode(4,34,90.0,"object category","obc")
  val bond_facet = add_slipnode(36,26,90.0,"bond facet","bf")

  // specify the descriptor types that bonds can form between
  var bond_facets = List(letter_category).to[ListBuffer]
  bond_facets += length

  // add initially_clamped_slipnodes
  var initially_clamped_slipnodes = List(letter_category).to[ListBuffer]
  letter_category.clamp=true;
  initially_clamped_slipnodes += string_position_category
  string_position_category.clamp=true;


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
    add_category_link(slipnet_letters(i), letter_category, letter_category.conceptual_depth-slipnet_letters(i).conceptual_depth);
    add_instance_link(letter_category, slipnet_letters(i), 97.0);
  }
  add_category_link(samegrp,letter_category,50.0);

  // *************** length links
  linkWith(slipnet_numbers, length, 100.0)

  add_nonslip_link(predgrp, length, 95.0);
  add_nonslip_link(succgrp, length, 95.0);
  add_nonslip_link(samegrp, length, 95.0);

  // *************** opposite links
  add_slip_link(first, last, opposite);
  add_slip_link(last, first, opposite);
  add_slip_link(leftmost, rightmost, opposite);
  add_slip_link(rightmost,leftmost, opposite);
  add_slip_link(left,right, opposite);
  add_slip_link(right,left, opposite);
  add_slip_link(successor,predecessor, opposite);
  add_slip_link(predecessor,successor, opposite);
  add_slip_link(succgrp,predgrp, opposite);
  add_slip_link(predgrp,succgrp, opposite);

  // ***************** has property links
  add_property_link(slipnet_letters(0),first, 75.0)
  add_property_link(slipnet_letters(25),last, 75.0)

  // ******************* object category links
  add_category_link(letter,object_category, object_category.conceptual_depth-letter.conceptual_depth)
  add_instance_link(object_category, letter, 100.0)
  add_category_link(group,object_category, object_category.conceptual_depth-group.conceptual_depth)
  add_instance_link(object_category, group, 100.0)

  // string position links
  add_category_link(leftmost, string_position_category,string_position_category.conceptual_depth-leftmost.conceptual_depth)
  add_instance_link(string_position_category, leftmost,100.0)
  add_category_link(rightmost, string_position_category,string_position_category.conceptual_depth-rightmost.conceptual_depth)
  add_instance_link(string_position_category, rightmost,100.0)
  add_category_link(middle, string_position_category,string_position_category.conceptual_depth-middle.conceptual_depth);
  add_instance_link(string_position_category, middle,100.0);
  add_category_link(single, string_position_category,string_position_category.conceptual_depth-single.conceptual_depth);
  add_instance_link(string_position_category, single,100.0);
  add_category_link(whole, string_position_category,string_position_category.conceptual_depth-whole.conceptual_depth);
  add_instance_link(string_position_category, whole,100.0);

  // alphabetic position category
  add_category_link(first, alphabetic_position_category,alphabetic_position_category.conceptual_depth-first.conceptual_depth);
  add_instance_link(alphabetic_position_category,first,100.0);
  add_category_link(last, alphabetic_position_category,alphabetic_position_category.conceptual_depth-last.conceptual_depth);
  add_instance_link(alphabetic_position_category,last,100.0);

  // direction-category links
  add_category_link(left, direction_category,direction_category.conceptual_depth-left.conceptual_depth);
  add_instance_link(direction_category,left,100.0);
  add_category_link(right, direction_category,direction_category.conceptual_depth-right.conceptual_depth);
  add_instance_link(direction_category,right,100.0);

  // bond-category links
  add_category_link(predecessor, bond_category,bond_category.conceptual_depth-predecessor.conceptual_depth);
  add_instance_link(bond_category,predecessor,100.0);
  add_category_link(successor, bond_category,bond_category.conceptual_depth-successor.conceptual_depth);
  add_instance_link(bond_category,successor,100.0);
  add_category_link(sameness, bond_category,bond_category.conceptual_depth-sameness.conceptual_depth);
  add_instance_link(bond_category,sameness,100.0);

  // group-category links
  add_category_link(predgrp, group_category,group_category.conceptual_depth-predgrp.conceptual_depth);
  add_instance_link(group_category,predgrp,100.0);
  add_category_link(succgrp, group_category,group_category.conceptual_depth-succgrp.conceptual_depth);
  add_instance_link(group_category,succgrp,100.0);
  add_category_link(samegrp, group_category,group_category.conceptual_depth-samegrp.conceptual_depth);
  add_instance_link(group_category,samegrp,100.0);

  // associated-group links
  add_nonslip_link(sameness,samegrp,group_category).fixed_length = 30.0
  add_nonslip_link(successor,succgrp,group_category).fixed_length = 60.0
  add_nonslip_link(predecessor,predgrp,group_category).fixed_length = 60.0

  // associated bond links
  add_nonslip_link(samegrp,sameness,bond_category).fixed_length = 90.0
  add_nonslip_link(succgrp,successor,bond_category).fixed_length = 90.0
  add_nonslip_link(predgrp,predecessor,bond_category).fixed_length = 90.0

  // bond facet links
  add_category_link(letter_category,bond_facet,bond_facet.conceptual_depth-letter_category.conceptual_depth);
  add_instance_link(bond_facet,letter_category,100.0);
  add_category_link(length,bond_facet,bond_facet.conceptual_depth-length.conceptual_depth);
  add_instance_link(bond_facet,length,100.0);

  // letter category links
  add_slip_link(letter_category,length,95.0);
  add_slip_link(length,letter_category,95.0);

  // letter group links
  add_slip_link(letter,group,90.0);
  add_slip_link(group,letter,90.0);

  // direction-position, direction-neighbor, position-neghbor links
  add_nonslip_link(left,leftmost,90.0);
  add_nonslip_link(leftmost,left,90.0);
  add_nonslip_link(right,leftmost,100.0);
  add_nonslip_link(leftmost,right,100.0);
  add_nonslip_link(right,rightmost,90.0);
  add_nonslip_link(rightmost,right,90.0);
  add_nonslip_link(left,rightmost,100.0);
  add_nonslip_link(rightmost,left,100.0);
  add_nonslip_link(leftmost,first,100.0);
  add_nonslip_link(first,leftmost,100.0);
  add_nonslip_link(rightmost,first,100.0);
  add_nonslip_link(first,rightmost,100.0);
  add_nonslip_link(leftmost,last,100.0);
  add_nonslip_link(last,leftmost,100.0);
  add_nonslip_link(rightmost,last,100.0);
  add_nonslip_link(last,rightmost,100.0);

  // other links
  add_slip_link(single,whole,90.0);
  add_slip_link(whole, single,90.0);




  // help methods

  def linkSuccessiveSlipNodes(nodes: ListBuffer[SlipNode]) = for (i <- 0 to nodes.size - 2) {
    add_nonslip_link(nodes(i), nodes(i+1), successor);
    add_nonslip_link(nodes(i+1), nodes(i), predecessor);
  }
  def linkWith(nodes: ListBuffer[SlipNode], withSplipNode: SlipNode, len: Double) = for (i <- 0 to nodes.size - 1) {
    add_category_link(nodes(i), withSplipNode, withSplipNode.conceptual_depth-nodes(i).conceptual_depth);
    add_instance_link(withSplipNode, nodes(i), len);
  }


  def add_category_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    sliplinks += nl
    fr.category_links += nl
    to.incoming_links += nl
    nl
  }

  def add_instance_link(fr: SlipNode, to: SlipNode, lab:SlipNode): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    sliplinks += nl
    fr.instance_links += nl
    to.incoming_links += nl
    nl;
  }
  def add_instance_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    sliplinks += nl
    fr.instance_links += nl
    to.incoming_links += nl
    nl;
  }


  def add_property_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    sliplinks += nl
    fr.has_property_links += nl
    to.incoming_links += nl
    nl;
  }



  def add_nonslip_link(fr: SlipNode, to: SlipNode, lab: SlipNode): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    sliplinks += nl
    fr.lateral_nonslip_links += nl
    to.incoming_links += nl
    nl;
  }
  def add_nonslip_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    sliplinks += nl
    fr.lateral_nonslip_links += nl
    to.incoming_links += nl
    nl;
  }


  def add_slip_link(fr: SlipNode, to: SlipNode, lab: SlipNode): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    nl.slip_link = true;

    sliplinks += nl
    fr.lateral_slip_links += nl
    to.incoming_links += nl
    nl;
  }
  def add_slip_link(fr: SlipNode, to: SlipNode, lab: Double): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    nl.slip_link = true;

    sliplinks += nl
    fr.lateral_slip_links += nl
    to.incoming_links += nl
    nl;
  }



  def add_slipnode(x: Int, y: Int, cd: Double, pn: String, sn: String) : SlipNode = {
    val slipNode = new SlipNode(x*25,y*25,cd,pn,sn)
    slipNodes += slipNode
    slipNode
  }


  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
    }
    case SetCoderack(cr) =>
      coderack = cr
    case BondFromTo(from, to) =>
      val bondFacetOpt = chooseBondFacet(from, to)
      bondFacetOpt match {
        case None =>
          log.debug(s" no possible bond-facet - fizzle")
        case Some(bondFacet) =>
          log.debug(s"chosen bond facet: ${bondFacet.name}")
          val fromDescriptorOpt = getDescriptor(from,bondFacet)
          val toDescriptorOpt = getDescriptor(to,bondFacet)
          if ((fromDescriptorOpt.isEmpty)||(toDescriptorOpt.isEmpty)){
            log.debug(" no possible bond-facet - fizzle")
          } else {
            val fromDescriptor = fromDescriptorOpt.get
            val toDescriptor = toDescriptorOpt.get
            log.debug(s"from object descriptor: ${fromDescriptor.name}")
            log.debug(s"to object descriptor: ${toDescriptor.name}")
            val bondCategoryOpt = getBondCategory(fromDescriptor, toDescriptor)
            bondCategoryOpt match {
              case None =>
                log.debug(" no suitable link - fizzle")
              case Some(bondCategory) =>
            }
          }

      }
  }

  def getDescriptor(workspaceObject: WorkspaceObject, node: SlipNode): Option[SlipNode] = ???
  def getBondCategory(fromDescriptor: SlipNode, toDescriptor: SlipNode): Option[SlipNode] = ???

  def chooseBondFacet(from: WorkspaceObject, to: WorkspaceObject): Option[SlipNode] = {
    val fromDescriptionFacets = facetsOfAndPartOf(from, bondFacets)
    val toDescriptionFacets = facetsOfAndPartOf(to, fromDescriptionFacets)
    if (toDescriptionFacets.isEmpty) {
      Option.empty[SlipNode]
    } else {
      val probs = toDescriptionFacets.map(sn => {
        total_description_type_support(sn, from.workspaceString())
      })
      val index = Utilities.valueProportionalRandomIndexInValueList(probs)
      Some(toDescriptionFacets(index))
    }
  }

  def total_description_type_support(slipNode: SlipNode, workspaceString: WorkspaceString): Double = {
    0.0
  }

  def facetsOfAndPartOf(wo: WorkspaceObject, facets: List[SlipNode]): List[SlipNode] = {
    wo.descriptions.filter(d => {
      val dTypeOpt = d.descriptionType
      dTypeOpt.isDefined && facets.contains(dTypeOpt)
    }).map(d => d.descriptionType).flatten
  }
}


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
  var incoming_links = ListBuffer.empty[SlipnetLink]
  val outgoing_links = ListBuffer.empty[SlipnetLink]
  val instance_links = ListBuffer.empty[SlipnetLink]
  val category_links = ListBuffer.empty[SlipnetLink]

  var clamp = false
}
class SlipnetLink(from_node: SlipNode, to_node: SlipNode, label: SlipNode, fixed_length: Double) {

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
  var slipnetNumbers = numbers.map(c => new SlipNode(0,0, 30.0, c, c)).to[ListBuffer]

  var slipNodes = slipnet_letters ++ slipnetNumbers



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
  linkSuccessiveSlipNodes(slipnetNumbers)
  // ************** letter category links
  for (i <- 0 to slipnet_letters.size - 1) {
    add_category_link(slipnet_letters(i), letter_category, letter_category.conceptual_depth-slipnet_letters(i).conceptual_depth);
    add_instance_link(letter_category, slipnet_letters(i), 97.0);
  }



  def linkSuccessiveSlipNodes(nodes: ListBuffer[SlipNode]) = for (i <- 0 to nodes.size - 2) {
    add_nonslip_link(nodes(i), nodes(i+1), successor);
    add_nonslip_link(nodes(i+1), nodes(i), predecessor);
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


  def add_nonslip_link(fr: SlipNode, to: SlipNode, lab: SlipNode): SlipnetLink = {
    val nl = new SlipnetLink(fr,to,lab);
    sliplinks += nl
    fr.lateral_nonslip_links += nl
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


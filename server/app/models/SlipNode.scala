package models

import akka.actor.ActorRef
import models.SlipNode.SlipNodeRep
import models.Workspace.SlipnodeActivationChanged

import scala.collection.mutable.ListBuffer

object SlipNode {
  var clamp_bdoa = false;

  case class SlipnetInfo(
                          slipnetOpposite: SlipNode,
                          slipnetSameness: SlipNode,
                          slipnetIdentity: SlipNode,
                          slipnetLetter: SlipNode,
                          slipnetGroup: SlipNode,
                          slipnetWhole: SlipNode,
                          slipnet_numbers: List[SlipNode]
                        )
  case class GroupSlipnetInfo(
                               bond_facet: SlipNodeRep,
                               bond_category: SlipNodeRep,
                               object_category: SlipNodeRep,
                               group: SlipNodeRep,
                               group_category: SlipNodeRep,
                               direction_category: SlipNodeRep,
                               string_position_category: SlipNodeRep,
                               whole: SlipNodeRep,
                               leftmost: SlipNodeRep,
                               rightmost: SlipNodeRep,
                               middle: SlipNodeRep,
                               length: SlipNodeRep,
                               samegrp: SlipNodeRep,
                               letter: SlipNodeRep,
                               letter_category: SlipNodeRep,
                               right: SlipNodeRep,
                               left: SlipNodeRep,
                               slipnet_numbers: List[SlipNodeRep]
                             )


  case class SlipNodeRep(id: String,
                         conceptual_depth: Double
                        )

  def displayStringWithOptionalSlipNodeRep(slipNodeRepOpt: Option[SlipNodeRep]) = slipNodeRepOpt match {
    case Some(sn) => SlipNode.longNameWithId(sn.id)
    case None => ""
  }


  def longNameWithId(id: String) = {
    if (id == SlipNode.id.letter) {
      "letter"
    } else if (id == SlipNode.id.group) {
      "group"
    } else if (id == SlipNode.id.leftmost) {
      "leftmost"
    } else if (id == SlipNode.id.right) {
      "right"
    } else if (id == SlipNode.id.left) {
      "left"
    } else if (id == SlipNode.id.rightmost) {
      "rightmost"
    } else if (id == SlipNode.id.leftmost) {
      "leftmost"
    } else if (id == SlipNode.id.whole) {
      "whole"
    } else if (id == SlipNode.id.middle) {
      "middle"
    } else if (id == SlipNode.id.letter_category) {
      "letter category"
    } else if (id == SlipNode.id.successor) {
      "successor"
    } else if (id == SlipNode.id.successor_group) {
      "successor group"
    } else {
      id
    }

  }

  object id {
    val right = "rt"
    val left = "lf"
    val sameness = "sm"
    val successor = "sc"
    val predecessor = "pd"
    val successor_group = "sg"
    val whole = "wh"
    val letter = "l"
    val group = "g"
    val letter_category = "lc"
    val bond_category = "bc"
    val leftmost = "lm"
    val rightmost = "rm"
    val middle = "md"
  }
}

class SlipNode(x: Int, y: Int, var conceptual_depth: Double, val name: String, val shortName: String, workspace: ActorRef) {
  var codelets = ListBuffer.empty[String]
  var lateral_nonslip_links = ListBuffer.empty[SlipnetLink]
  var lateral_slip_links = ListBuffer.empty[SlipnetLink]
  var incoming_links = ListBuffer.empty[SlipnetLink]
  val outgoing_links = ListBuffer.empty[SlipnetLink]
  val instance_links = ListBuffer.empty[SlipnetLink]
  val category_links = ListBuffer.empty[SlipnetLink]
  val has_property_links = ListBuffer.empty[SlipnetLink]

  var activation: Double = 0.0
  var buffer: Double = 0.0
  var old_activation: Double = 0.0


  var clamp = false
  var oldclamp = false

  var intrinsic_link_length = 0.0
  var shrunk_link_length = 0.0

  def this(x: Int, y: Int, conceptual_depth: Double, name: String, shortName: String, len: Double, workspace: ActorRef) = {
    this(x, y, conceptual_depth, name, shortName, workspace)
    intrinsic_link_length = len;
    shrunk_link_length = len*0.4;
  }

  override def toString(): String  = {
     "slipnode "+ id() + " activation "+ activation +" buffer "+buffer+" conceptual_depth "+conceptual_depth
  }


  def id() = shortName

  def slipNodeRep() = SlipNodeRep(id(), conceptual_depth)

  def category(): Option[SlipNode] = {
    if (category_links.isEmpty) None else {
      val sl = category_links(0)
      Some(sl.to_node)
    }
  }

  def setBuffer(value: Double) = {
//    println(s"setBuffer slipnode id ${id()} buffer $value")
    buffer = value
  }
  def setActivation(value: Double) = {
    if (!activation.equals(value)) {
      activation = value
//      println(s"setActivation slipnode id ${id()} activation $activation")
      workspace ! SlipnodeActivationChanged(id(),activation)
    }
  }

  def degree_of_association(): Double = {
    // used in calculating link lengths
    val dof = if (activation==100.0) shrunk_link_length else intrinsic_link_length
    100.0-dof
  }

  def bond_degree_of_association(): Double = {
    // used to calculate strength of bonds of this type
    println("bond_degree_of_association " + id() + " clamp_bdoa: " + SlipNode.clamp_bdoa + " activation: " + activation + " shrunk_link_length: " + shrunk_link_length + " intrinsic_link_length: " + intrinsic_link_length);

    val dof1 = if ((!SlipNode.clamp_bdoa)&&(activation==100.0)) shrunk_link_length else intrinsic_link_length

    val dof = Math.sqrt(100.0-dof1) *11.0   /// usually *11.0- but lets see what happens
    if (dof>100.0) 100.0 else dof
  }

}

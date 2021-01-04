package models

import akka.actor.ActorRef
import models.SlipNode.SlipNodeRep

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


  case class SlipNodeRep(id: String, activation: Double)

  object id {
    val right = "rt"
    val sameness = "sm"
    val whole = "wh"
    val letter = "l"
    val group = "g"
    val letter_category = "lc"
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

  def id() = shortName

  def slipNodeRep() = SlipNodeRep(id(), activation)

  def category(): Option[SlipNode] = {
    if (category_links.isEmpty) None else {
      val sl = category_links(0)
      Some(sl.to_node)
    }
  }

  def setActivation(value: Double) = {
    if (activation != value) {
      activation = value
      //workspace ! SlipnodeActivationChanged(id(),activation)
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

package models

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

  case class SlipNodeRep(id: String, activation: Double)
}

class SlipNode(x: Int, y: Int, val conceptual_depth: Double, val name: String, val shortName: String) {
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

  def this(x: Int, y: Int, conceptual_depth: Double, name: String, len: Double) = {
    this(x, y, conceptual_depth, name, name)
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


  def degree_of_association(): Double = {
    // used in calculating link lengths
    val dof = if (activation==100.0) shrunk_link_length else intrinsic_link_length
    100.0-dof
  }

  def bond_degree_of_association(): Double = {
    // used to calculate strength of bonds of this type
    val dof1 = if ((!SlipNode.clamp_bdoa)&&(activation==100.0)) shrunk_link_length else intrinsic_link_length

    val dof = Math.sqrt(100.0-dof1) *11.0   /// usually *11.0- but lets see what happens
    if (dof>100.0) 100.0 else dof
  }

}

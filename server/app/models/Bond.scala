package models

import akka.actor.ActorRef
import models.SlipNode.SlipNodeRep
import models.Slipnet.SetSlipNodeBufferValue

object Bond {
  case class BondRep(from_obj: String,
                     to_obj: String,
                     bondCategorySlipNodeID: String,
                     bondFacetSlipNodeID: String,
                     from_obj_descriptorSlipNodeID: String,
                     to_obj_descriptorSlipNodeID: String)

}

class Bond (
             val from_obj: WorkspaceObject,
             val to_obj: WorkspaceObject,
             val bond_category: SlipNodeRep,
             val bond_facet: SlipNodeRep,
             val from_obj_descriptor: SlipNodeRep,
             val to_obj_descriptor: SlipNodeRep,
             slipnetLeft: SlipNodeRep,
             slipnetRight: SlipNodeRep,
             slipnet: ActorRef
           ) extends WorkspaceStructure {

  import Bond.BondRep

  val leftGreater = from_obj.left_string_position > to_obj.right_string_position
  val left_obj: WorkspaceObject = if (leftGreater) to_obj else from_obj
  val right_obj: WorkspaceObject = if (leftGreater) from_obj else to_obj

  val direction_category: SlipNodeRep = if (leftGreater) slipnetLeft else slipnetRight
  val bidirectional = from_obj_descriptor == to_obj_descriptor // true if sameness bond
  val right = to_obj == right_obj // true if to_obj is on the right

  wString = from_obj.wString


  def bondRep(): BondRep = BondRep(
    from_obj.uuid,
    to_obj.uuid,
    bond_category.id,
    bond_facet.id,
    from_obj_descriptor.id,
    to_obj_descriptor.id
  )

  def get_incompatible_bonds(): List[Bond] = {
    workspaceString().get.bonds().filter(b =>
      (b.left_obj == left_obj) || (b.right_obj == right_obj)
    )
  }

  // partially moved to Workspace.addBond
  def build_bond() = {
    workspaceString().get.addBond(this);

    activateDescriptor()
    left_obj.right_bond = Some(this);
    right_obj.left_bond = Some(this);
    left_obj.addBond(this)
    right_obj.addBond(this)
  }

  // partially moved to Workspace.break_bond
  def break_bond() = {
    if (workspaceString().isDefined) workspaceString().get.break_bond(this)
    left_obj.right_bond = null;
    right_obj.left_bond = null;
    left_obj.break_bond(this);
    right_obj.break_bond(this);
// GUI   workspace.WorkspaceArea.Redraw = true;
  }


  def activateDescriptor() = {
    slipnet ! SetSlipNodeBufferValue(bond_category.id, 100.0)
    // ??? if (direction_category.is) direction_category.buffer=100.0;
    slipnet ! SetSlipNodeBufferValue(direction_category.id, 100.0)
  }

  def correspondenceInDirection(
    cwo: WorkspaceObject,
    dirMost: WorkspaceObject => Boolean,
    sideBond: WorkspaceObject => Option[Bond],
    initial: WorkspaceString): Option[Correspondence] = {
    if ((dirMost(left_obj)) && (cwo.correspondence.isDefined)) {
      val locOpt = cwo.correspondence;
      if (locOpt.isDefined) {
        val loc = locOpt.get
        val wo = if (workspaceString() == initial) loc.obj2 else loc.obj1
        if ((wo.leftmost) && (wo.right_bond.isDefined)) {
          if (sideBond(wo).isDefined) {
            val rb = sideBond(wo).get
            // ignore test on rb.direction_category != null
            if (rb.direction_category != direction_category) {
              Some(loc)
            } else None
          } else None
        } else None
      } else None
    } else None
  }

  def get_incompatible_correspondences(initial: WorkspaceString): List[Correspondence] = {
    // returns a list of correspondences that are incompatible with this bond
    val locOpt = correspondenceInDirection(
      left_obj,
      (wo: WorkspaceObject) => wo.leftmost,
      (wo: WorkspaceObject) => wo.right_bond,
      initial)
    val rocOpt = correspondenceInDirection(
      right_obj,
      (wo: WorkspaceObject) => wo.rightmost,
      (wo: WorkspaceObject) => wo.left_bond,
      initial)
    List(locOpt,rocOpt).flatten
  }

}
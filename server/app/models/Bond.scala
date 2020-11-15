package models

import models.SlipNode.SlipNodeRep

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
             slipnetRight: SlipNodeRep
           ) extends WorkspaceStructure {

  import Bond.BondRep

  val leftGreater = from_obj.left_string_position>to_obj.right_string_position
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
}
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
             from_obj: WorkspaceObject,
             to_obj: WorkspaceObject,
             bondCategory: SlipNodeRep,
             bondFacet: SlipNodeRep,
             from_obj_descriptor: SlipNodeRep,
             to_obj_descriptor: SlipNodeRep
           ) extends WorkspaceStructure {

  import Bond.BondRep

  def bondRep(): BondRep = BondRep(
    from_obj.uuid,
    to_obj.uuid,
    bondCategory.id,
    bondFacet.id,
    from_obj_descriptor.id,
    to_obj_descriptor.id
  )
}
package models

import models.Slipnet.BondRep

class Bond (
             from_obj: WorkspaceObject,
             to_obj: WorkspaceObject,
             bondCategorySlipNodeID: String,
             bondFacetSlipNodeID: String,
             from_obj_descriptorSlipNodeID: String,
             to_obj_descriptorSlipNodeID: String
           ) extends WorkspaceStructure {

  def bondRep(): BondRep = BondRep(
    from_obj.uuid,
    to_obj.uuid,
    bondCategorySlipNodeID,
    bondFacetSlipNodeID,
    from_obj_descriptorSlipNodeID,
    to_obj_descriptorSlipNodeID
  )
}
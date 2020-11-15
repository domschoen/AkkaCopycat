package models

import models.Slipnet.{GroupRep, WorkspaceStructureRep}

import scala.collection.mutable.ListBuffer

class Group (
              wString: WorkspaceString,
              groupCategorySlipNodeID: String,
              directionCategorySlipNodeID: String,
              bondFacetSlipNodeID: String,
              object_list: ListBuffer[WorkspaceObject],
              bond_list: ListBuffer[Bond]
            ) extends WorkspaceObject(wString) {

  override  def workspaceStructureRep(): WorkspaceStructureRep = {
    val bondReps = bond_list.toList.map(b => b.bondRep())
    val groupRep = GroupRep(groupCategorySlipNodeID, directionCategorySlipNodeID, bondFacetSlipNodeID, bondReps)

    WorkspaceStructureRep(uuid,descriptionReps(),letterOrGroupCompanionReps(), spans_string, None)
  }

}

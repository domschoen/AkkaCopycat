package models

import akka.actor.ActorRef
import models.Slipnet.{GroupRep, WorkspaceStructureRep}

import scala.collection.mutable.ListBuffer

class Group (
              wString: WorkspaceString,
              val groupCategorySlipNodeID: String,
              val directionCategorySlipNodeID: String,
              val bondFacetSlipNodeID: String,
              var object_list: ListBuffer[WorkspaceObject],
              var bond_list: ListBuffer[Bond],
              slipnet: ActorRef
            ) extends WorkspaceObject(wString) {
  import Slipnet.SetSlipNodeBufferValue

  override  def workspaceStructureRep(): WorkspaceStructureRep = {
    val bondReps = bond_list.toList.map(b => b.bondRep())
    val groupRep = GroupRep(groupCategorySlipNodeID, directionCategorySlipNodeID, bondFacetSlipNodeID, bondReps)

    WorkspaceStructureRep(uuid,descriptionReps(),letterOrGroupCompanionReps(), spans_string, None)
  }


  def activate_descriptions() = {
    for (d <- descriptions) {
      d.descriptor match {
        case Some(descriptor) =>
          slipnet ! SetSlipNodeBufferValue(descriptor.id, 100.0)

        case None =>
          println("oup oups !");
      }
    }
  }
  def break_group() = {
    for (wo <- object_list) {
      wo.group = None
    }
    if (workspaceString().isDefined) {
      workspaceString().get.break_group(this)
    }
  }
}

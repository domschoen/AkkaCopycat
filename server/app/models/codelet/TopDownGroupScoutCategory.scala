package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Slipnet.DirValue.DirValue
import models.SlipNode.SlipNodeRep
import models.Slipnet.{CompleteProposeGroupResponse, SlipnetGoWithTopDownGroupScoutCategory, SlipnetGoWithTopDownGroupScoutCategory2, WorkspaceStructureRep}
import models.Workspace.WorkspaceProposeGroup

object TopDownGroupScoutCategory {
  case class SlipnetGoWithTopDownGroupScoutCategoryResponse(bond_category: SlipNodeRep)
  case class GoWithTopDownGroupScoutCategoryResponse(direction: DirValue, fromob: WorkspaceStructureRep)
  case class SlipnetGoWithTopDownGroupScoutCategory2Response(direction: DirValue, lengthActivation: Double)
  case class GoWithTopDownGroupScoutCategory2Response(
                                                       wStringFromWo: WorkspaceStructureRep,
                                                       group_category: String,
                                                       direction_category: Option[String],
                                                       bond_facet: String,
                                                       object_list: List[WorkspaceStructureRep],
                                                       bond_list: List[WorkspaceStructureRep]
                                                     )
}
class TopDownGroupScoutCategory(urgency: Int,
                                workspace: ActorRef,
                                slipnet: ActorRef,
                                temperature: ActorRef,
                                arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeGroup
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import TopDownGroupScoutCategory.{
    SlipnetGoWithTopDownGroupScoutCategoryResponse,
    GoWithTopDownGroupScoutCategoryResponse,
    SlipnetGoWithTopDownGroupScoutCategory2Response,
    GoWithTopDownGroupScoutCategory2Response
  }
  import models.Workspace.{
    GoWithTopDownGroupScoutCategory,
    GoWithTopDownGroupScoutCategory2,
    WorkspaceProposeGroupResponse
  }
  import models.Slipnet.{
    CompleteProposeGroup
  }

  var fromob: WorkspaceStructureRep = null
  var bond_category: SlipNodeRep = null
  var runTemperature: Double = 0.0
  var wStringFromWo: WorkspaceStructureRep = null
  var group_category: String = null
  var direction_category: Option[String] = None
  var bond_facet: String = null
  var object_list =  List.empty[WorkspaceStructureRep]
  var bond_list = List.empty[WorkspaceStructureRep]
  var groupUrgency = 0.0

  def groupID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      log.debug("trying to build "+groupID+" group");

      slipnet ! SlipnetGoWithTopDownGroupScoutCategory(groupID, runTemperature)

    case SlipnetGoWithTopDownGroupScoutCategoryResponse(bc) =>
      bond_category = bc
      workspace ! GoWithTopDownGroupScoutCategory(groupID(), bond_category, runTemperature)

    case GoWithTopDownGroupScoutCategoryResponse(direction, fb) =>
      fromob = fb
      slipnet ! SlipnetGoWithTopDownGroupScoutCategory2(direction)

    case SlipnetGoWithTopDownGroupScoutCategory2Response(direction, lengthActivation) =>
      workspace ! GoWithTopDownGroupScoutCategory2(direction, fromob, bond_category, runTemperature, lengthActivation)

    case GoWithTopDownGroupScoutCategory2Response(ws, gc, dc, bf, ol, bl) =>
      wStringFromWo = ws
      group_category = gc
      direction_category = dc
      bond_facet = bf
      object_list = ol
      bond_list = bl

      slipnet ! CompleteProposeGroup(group_category, direction_category)

    case CompleteProposeGroupResponse(u) =>
      groupUrgency = u
      workspace ! WorkspaceProposeGroup(wStringFromWo, group_category, direction_category, bond_facet, object_list, bond_list)


    case WorkspaceProposeGroupResponse(groupID) =>
      coderack ! ProposeGroup(groupID, groupUrgency)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step
  }


}


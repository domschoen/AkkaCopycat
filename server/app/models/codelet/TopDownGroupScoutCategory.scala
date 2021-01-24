package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Bond.BondRep
import models.SlipNode.{GroupSlipnetInfo, SlipNodeRep}
import models.Slipnet.{CompleteProposeGroupResponse, SlipnetGoWithTopDownGroupScoutCategory, SlipnetGoWithTopDownGroupScoutCategory2, SlipnetGoWithTopDownGroupScoutCategory3}
import models.Workspace.WorkspaceProposeGroup
import models.WorkspaceObject.WorkspaceObjectRep

object TopDownGroupScoutCategory {
  case class SlipnetGoWithTopDownGroupScoutCategoryResponse(bond_category: SlipNodeRep, groupSlipnetInfo: GroupSlipnetInfo)
  case class GoWithTopDownGroupScoutCategoryResponse(direction: Option[SlipNodeRep], fromob: WorkspaceObjectRep)
  case class SlipnetGoWithTopDownGroupScoutCategory2Response(group_category: SlipNodeRep, mydirection: SlipNodeRep)
  case class GoWithTopDownGroupScoutCategory2Response(
                                                       group_category: SlipNodeRep,
                                                       direction_category: Option[SlipNodeRep],
                                                       bond_facet: SlipNodeRep,
                                                       object_list: List[WorkspaceObjectRep],
                                                       bond_list: List[BondRep]
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

  var fromob: WorkspaceObjectRep = null
  var bond_category: SlipNodeRep = null
  var runTemperature: Double = 0.0
  var group_category: SlipNodeRep = null
  var direction_category: Option[SlipNodeRep] = None
  var bond_facet: SlipNodeRep = null
  var object_list =  List.empty[WorkspaceObjectRep]
  var bond_list = List.empty[BondRep]
  var groupUrgency = 0.0
  var groupSlipnetInfo: GroupSlipnetInfo = null

  def groupID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      log.debug("trying to build "+groupID+" group");

      slipnet ! SlipnetGoWithTopDownGroupScoutCategory(groupID, runTemperature)

    case SlipnetGoWithTopDownGroupScoutCategoryResponse(bc, gsi) =>
      bond_category = bc
      groupSlipnetInfo = gsi

      workspace ! GoWithTopDownGroupScoutCategory(bond_category.id, "bond_category", runTemperature, groupSlipnetInfo)

    case GoWithTopDownGroupScoutCategoryResponse(direction, fb) =>
      log.debug("GoWithTopDownGroupScoutCategoryResponse")
      fromob = fb
      slipnet ! SlipnetGoWithTopDownGroupScoutCategory2(groupID, direction)

    case SlipnetGoWithTopDownGroupScoutCategory2Response(group_category, mydirection) =>
      workspace ! GoWithTopDownGroupScoutCategory2(group_category, mydirection, fromob, bond_category, runTemperature, groupSlipnetInfo)

    case GoWithTopDownGroupScoutCategory2Response(gc, dc, bf, ol, bl) =>
      group_category = gc
      direction_category = dc
      bond_facet = bf
      object_list = ol
      bond_list = bl

      slipnet ! CompleteProposeGroup(group_category, direction_category)

    case CompleteProposeGroupResponse(u, bond_category) =>
      log.debug("TopDownGroupScoutCategory. CompleteProposeGroupResponse")
      groupUrgency = u
      workspace ! WorkspaceProposeGroup(
        object_list, bond_list, group_category, direction_category, bond_facet, bond_category,groupSlipnetInfo,runTemperature)


    case WorkspaceProposeGroupResponse(groupID) =>
      coderack ! ProposeGroup(groupID, groupUrgency)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)
  }


}


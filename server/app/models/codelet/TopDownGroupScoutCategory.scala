package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Bond.BondRep
import models.Slipnet.DirValue.DirValue
import models.SlipNode.SlipNodeRep
import models.Slipnet.{CompleteProposeGroupResponse, SlipnetGoWithTopDownGroupScoutCategory, SlipnetGoWithTopDownGroupScoutCategory2, SlipnetGoWithTopDownGroupScoutCategory3}
import models.Workspace.WorkspaceProposeGroup
import models.WorkspaceObject.WorkspaceObjectRep

object TopDownGroupScoutCategory {
  case class SlipnetGoWithTopDownGroupScoutCategoryResponse(bond_category: SlipNodeRep)
  case class GoWithTopDownGroupScoutCategoryResponse(direction: DirValue, fromob: WorkspaceObjectRep)
  case class SlipnetGoWithTopDownGroupScoutCategory2Response(direction: SlipNodeRep, lengthActivation: Double)
  case class GoWithTopDownGroupScoutCategory2Response(
                                                       group_category: String,
                                                       direction_category: Option[String],
                                                       bond_facet: String,
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
  var group_category: String = null
  var direction_category: Option[String] = None
  var bond_facet: String = null
  var object_list =  List.empty[WorkspaceObjectRep]
  var bond_list = List.empty[BondRep]
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
      workspace ! GoWithTopDownGroupScoutCategory(bond_category.id, "bond_category", runTemperature)

    case GoWithTopDownGroupScoutCategoryResponse(direction, fb) =>
      fromob = fb
      slipnet ! SlipnetGoWithTopDownGroupScoutCategory2(direction)

    case SlipnetGoWithTopDownGroupScoutCategory2Response(direction, lengthActivation) =>
      workspace ! GoWithTopDownGroupScoutCategory2(groupID, direction, fromob, bond_category, runTemperature, lengthActivation)

    case GoWithTopDownGroupScoutCategory2Response(gc, dc, bf, ol, bl) =>
      group_category = gc
      direction_category = dc
      bond_facet = bf
      object_list = ol
      bond_list = bl

      slipnet ! CompleteProposeGroup(group_category, direction_category)

    case CompleteProposeGroupResponse(u) =>
      groupUrgency = u
      workspace ! WorkspaceProposeGroup(object_list, bond_list, group_category, direction_category, bond_facet)


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


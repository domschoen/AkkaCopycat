package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Bond.BondRep
import models.Coderack.ProposeGroup
import models.SlipNode.SlipNodeRep
import models.Slipnet.{CompleteProposeGroup, CompleteProposeGroupResponse, SlipnetGoWithTopDownGroupScoutCategory2, SlipnetGoWithTopDownGroupScoutDirection}
import models.Workspace.{GoWithTopDownGroupScoutCategory, GoWithTopDownGroupScoutCategory2, GoWithTopDownGroupScoutDirection, GoWithTopDownGroupScoutDirection2, WorkspaceProposeGroup, WorkspaceProposeGroupResponse}
import models.WorkspaceObject.WorkspaceObjectRep
import models.codelet.TopDownGroupScoutCategory.{GoWithTopDownGroupScoutCategory2Response, GoWithTopDownGroupScoutCategoryResponse, SlipnetGoWithTopDownGroupScoutCategory2Response}

object TopDownGroupScoutDirection {
  case class GoWithTopDownGroupScoutDirectionResponse(bond_category: SlipNodeRep, first_bond: String)
  case class SlipnetGoWithTopDownGroupScoutDirectionResponse(group_category: Option[SlipNodeRep])
}
class TopDownGroupScoutDirection(urgency: Int,
                                 workspace: ActorRef,
                                 slipnet: ActorRef,
                                 temperature: ActorRef,
                                 arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import models.codelet.TopDownGroupScoutDirection.{
    GoWithTopDownGroupScoutDirectionResponse,
    SlipnetGoWithTopDownGroupScoutDirectionResponse
  }

  var runTemperature: Double = 0.0
  var fromob: WorkspaceObjectRep = null
  var first_bond: String = null
  var bond_category: SlipNodeRep = null
  var group_category: String = null
  var direction_category: Option[String] = None
  var bond_facet: String = null
  var object_list =  List.empty[WorkspaceObjectRep]
  var bond_list = List.empty[BondRep]
  var groupUrgency = 0.0

  def directionID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      log.debug("looking for "+directionID+" group");

      workspace ! GoWithTopDownGroupScoutCategory(directionID, "direction_category", runTemperature)

    case GoWithTopDownGroupScoutCategoryResponse(direction, fb) =>
      fromob = fb
      slipnet ! SlipnetGoWithTopDownGroupScoutCategory2(direction)

    case SlipnetGoWithTopDownGroupScoutCategory2Response(direction, lengthActivation) =>
      workspace ! GoWithTopDownGroupScoutDirection(directionID, direction, fromob, runTemperature)

    case GoWithTopDownGroupScoutDirectionResponse(bc, fb) =>
      first_bond = fb
      bond_category = bc
      slipnet ! SlipnetGoWithTopDownGroupScoutDirection(bond_category)

    case SlipnetGoWithTopDownGroupScoutDirectionResponse(group_category) =>
      workspace ! GoWithTopDownGroupScoutDirection2(group_category, fromob, first_bond, bond_category)

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


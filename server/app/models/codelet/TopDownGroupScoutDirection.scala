package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Bond.BondRep
import models.Coderack.{ProposeGroup, Temperatures}
import models.SlipNode.{GroupSlipnetInfo, SlipNodeRep}
import models.Slipnet.{CompleteProposeGroup, CompleteProposeGroupResponse, SlipnetGoWithTopDownGroupScoutCategory2, SlipnetGoWithTopDownGroupScoutDirection, SlipnetGoWithTopDownGroupScoutDirection0}
import models.Workspace.{GoWithTopDownGroupScoutCategory, GoWithTopDownGroupScoutCategory2, GoWithTopDownGroupScoutDirection, GoWithTopDownGroupScoutDirection2, WorkspaceProposeGroup}
import models.WorkspaceObject.WorkspaceObjectRep
import models.codelet.TopDownGroupScoutCategory.{GoWithTopDownGroupScoutCategory2Response, GoWithTopDownGroupScoutCategoryResponse, SlipnetGoWithTopDownGroupScoutCategory2Response}


// Codelet.java.667

object TopDownGroupScoutDirection {
  case class SlipnetGoWithTopDownGroupScoutDirection0Response(groupSlipnetInfo: GroupSlipnetInfo)
  case class GoWithTopDownGroupScoutDirectionResponse(bond_category: SlipNodeRep, first_bond: String)
  //case class SlipnetGoWithTopDownGroupScoutDirectionResponse(group_category: Option[SlipNodeRep], groupSlipnetInfo: GroupSlipnetInfo)
}
class TopDownGroupScoutDirection(urgency: Int,
                                 workspace: ActorRef,
                                 slipnet: ActorRef,
                                 arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.codelet.TopDownGroupScoutDirection.{
    SlipnetGoWithTopDownGroupScoutDirection0Response,
    GoWithTopDownGroupScoutDirectionResponse
  }
  import models.Workspace.WorkspaceProposeGroupResponse

  var runTemperature: Temperatures = null
  var fromob: WorkspaceObjectRep = null
  var first_bond: String = null
  var bond_category: SlipNodeRep = null
  var direction_category: Option[SlipNodeRep] = None
  var bond_facet: SlipNodeRep = null
  var object_list =  List.empty[WorkspaceObjectRep]
  var bond_list = List.empty[BondRep]
  var groupUrgency = 0.0
  var groupSlipnetInfo : GroupSlipnetInfo = null

  def directionID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t
      log.debug("looking for "+directionID+" group")

      slipnet ! SlipnetGoWithTopDownGroupScoutDirection0

    case SlipnetGoWithTopDownGroupScoutDirection0Response(gsi) =>
      groupSlipnetInfo = gsi
      workspace ! GoWithTopDownGroupScoutCategory(directionID, "direction_category", runTemperature, groupSlipnetInfo)

    case GoWithTopDownGroupScoutCategoryResponse(direction, fb) =>
      fromob = fb
      slipnet ! SlipnetGoWithTopDownGroupScoutCategory2(directionID, direction)

    case SlipnetGoWithTopDownGroupScoutCategory2Response(slipNodeRep, mydirection) =>
      workspace ! GoWithTopDownGroupScoutDirection(slipNodeRep, mydirection, fromob, runTemperature, groupSlipnetInfo)

    case GoWithTopDownGroupScoutDirectionResponse(bc, fb) =>
      log.debug("GoWithTopDownGroupScoutDirectionResponse")
      first_bond = fb
      bond_category = bc
      slipnet ! SlipnetGoWithTopDownGroupScoutDirection(bond_category, fb, fromob.uuid)

    /*case SlipnetGoWithTopDownGroupScoutDirectionResponse(group_category, gsi) =>
      groupSlipnetInfo = gsi
      workspace ! GoWithTopDownGroupScoutDirection2(group_category, fromob, first_bond, bond_category)*/

    case GoWithTopDownGroupScoutCategory2Response(group_category, dc, bf, ol, bl) =>
      direction_category = dc
      bond_facet = bf
      object_list = ol
      bond_list = bl

      slipnet ! CompleteProposeGroup(group_category, direction_category)

    case CompleteProposeGroupResponse(u,bond_category, group_category) =>
      groupUrgency = u
      workspace ! WorkspaceProposeGroup(
        object_list, bond_list, group_category, direction_category, bond_facet,bond_category, groupSlipnetInfo, runTemperature)

    case WorkspaceProposeGroupResponse(groupID) =>
      coderack ! ProposeGroup(groupID, groupUrgency)


    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


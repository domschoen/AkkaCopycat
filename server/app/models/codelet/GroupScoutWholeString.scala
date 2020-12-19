package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Bond.BondRep
import models.Coderack.ProposeGroup
import models.Group.GroupRep
import models.SlipNode.{GroupSlipnetInfo, SlipNodeRep}
import models.Slipnet.{CompleteProposeGroup, CompleteProposeGroupResponse, GetRelatedNodeOf, GetRelatedNodeOfResponse, SlipnetGoWithGroupScoutWholeString}
import models.Workspace.WorkspaceProposeGroupResponse
import models.WorkspaceObject.WorkspaceObjectRep
import models.codelet.GroupScoutWholeString.GroupScoutWholeString2Response
import models.codelet.TopDownGroupScoutCategory.GoWithTopDownGroupScoutCategory2Response


// Codelet.java.789
object GroupScoutWholeString {
  case class GoWithGroupScoutWholeStringResponse(left_most: WorkspaceObjectRep)
  case class SlipnetGoWithGroupScoutWholeStringResponse(bond_category: Option[SlipNodeRep],
                                                        groupSlipnetInfo: GroupSlipnetInfo)

  case class GroupScoutWholeString2Response(
                                                       group_category: SlipNodeRep,
                                                       direction_category: Option[SlipNodeRep],
                                                       bond_facet: SlipNodeRep,
                                                       object_list: List[WorkspaceObjectRep],
                                                       bond_list: List[BondRep]
                                                     )


  case class GroupScoutWholeString3Response(
                                                       bond_category: SlipNodeRep,
                                                       direction_category: Option[SlipNodeRep],
                                                       bond_facet: SlipNodeRep,
                                                       object_list: List[WorkspaceObjectRep],
                                                       bond_list: List[BondRep]
                                                     )

  case class GetLeftAndRightResponse(slipnetLeft: SlipNodeRep, slipnetRight: SlipNodeRep)

}
class GroupScoutWholeString(urgency: Int,
                            workspace: ActorRef,
                            slipnet: ActorRef,
                            temperature: ActorRef,
                            arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import models.codelet.GroupScoutWholeString.{
    GoWithGroupScoutWholeStringResponse,
    SlipnetGoWithGroupScoutWholeStringResponse,
    GetLeftAndRightResponse
  }
  import models.Workspace.{
    GoWithGroupScoutWholeString,
    GoWithGroupScoutWholeString2,
    WorkspaceProposeGroup
  }
  import models.Slipnet.GetLeftAndRight

  var runTemperature: Double = 0.0
  var left_most: WorkspaceObjectRep = null
  var group_category: SlipNodeRep = null
  var direction_category: Option[SlipNodeRep] = None
  var bond_facet: SlipNodeRep = null
  var object_list =  List.empty[WorkspaceObjectRep]
  var bond_list = List.empty[BondRep]
  var groupUrgency = 0.0
  var slipnetLeft: SlipNodeRep = null
  var slipnetRight: SlipNodeRep = null
  var groupSlipnetInfo: GroupSlipnetInfo = null

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t

      slipnet ! GetLeftAndRight

    case GetLeftAndRightResponse(sl: SlipNodeRep, sr: SlipNodeRep) =>
      slipnetLeft = sl
      slipnetRight = sr
      workspace ! GoWithGroupScoutWholeString(runTemperature)

    case GoWithGroupScoutWholeStringResponse(lm) =>
      left_most = lm
      if (left_most.groupRep.isDefined) {
        slipnet ! GetRelatedNodeOf(left_most.groupRep.get.group_category.id, "bc")
      } else {
        workspace ! GoWithGroupScoutWholeString2(left_most, slipnetLeft, slipnetRight)
      }

    case GetRelatedNodeOfResponse(relatedOpt) =>
      relatedOpt match {
        case Some(related) =>
          if (related == "sm") { // sameness
            val leftmostGroupOpt = left_most.groupRep
            leftmostGroupOpt match {
              case Some(leftmostGroup) =>
                self ! GoWithGroupScoutWholeStringResponse(leftmostGroup.workspaceObjectRep)
              case None =>
                workspace ! GoWithGroupScoutWholeString2(left_most, slipnetLeft, slipnetRight)
            }
          }
        case None =>
          workspace ! GoWithGroupScoutWholeString2(left_most, slipnetLeft, slipnetRight)
      }

    case GroupScoutWholeString2Response(gc, dc, bf, ol, bl) =>
      group_category = gc
      direction_category = dc
      bond_facet = bf
      object_list = ol
      bond_list = bl

      slipnet ! CompleteProposeGroup(group_category, direction_category)

    case GroupScoutWholeString2Response(bc, dc, bf, ol, bl) =>
      direction_category = dc
      bond_facet = bf
      object_list = ol
      bond_list = bl
      slipnet ! SlipnetGoWithGroupScoutWholeString(bc)

    case SlipnetGoWithGroupScoutWholeStringResponse(relatedOpt, gsi) =>
      groupSlipnetInfo = gsi
      relatedOpt match {
        case Some(group_cat) =>
          slipnet ! CompleteProposeGroup(group_cat, direction_category)
        case None =>
          self ! Finished
      }

    case CompleteProposeGroupResponse(u, bc) =>
      groupUrgency = u
      workspace ! WorkspaceProposeGroup(
        object_list,
        bond_list,
        group_category,
        direction_category,
        bond_facet,
        bc,
        groupSlipnetInfo,
        runTemperature
      )

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


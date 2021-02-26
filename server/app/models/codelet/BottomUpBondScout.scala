package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import com.sun.org.apache.xerces.internal.impl.xpath.XPath.Step
import models.SlipNode.SlipNodeRep
import models.Slipnet.{BondFromTo, BondFromTo2}
import models.Workspace
import models.Workspace.{WorkspaceProposeBond, WorkspaceProposeBondResponse}
import models.WorkspaceObject.{WorkspaceObjectRep}
import models.WorkspaceStructure.WorkspaceStructureRep
import models.codelet.BottomUpBondScout.BondFromToSlipnetResponse
import models.codelet.Codelet.Finished

object BottomUpBondScout {
  case class GoWithBottomUpBondScoutResponse(from: WorkspaceObjectRep, to: WorkspaceObjectRep)
  case class BondFromToSlipnetResponse(fromFacets: List[SlipNodeRep], toFacets: List[SlipNodeRep])
  case class GoWithBottomUpBondScout2Response(
                                               bondFacet: SlipNodeRep,
                                               fromDescriptor: Option[SlipNodeRep],
                                               toDescriptor: Option[SlipNodeRep]
                                             )
  case class BondFromTo2Response(
                                  bondCategory: SlipNodeRep,
                                  bondCategoryDegreeOfAssociation: Double,
                                  slipnetLeft: SlipNodeRep,
                                  slipnetRight: SlipNodeRep
                                )
}


// codelet.java.240
class BottomUpBondScout(urgency: Int,              workspace: ActorRef,
                        slipnet: ActorRef,
                        arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.Run
  import Workspace.BondWithNeighbor
  import models.Coderack.{ChooseAndRun, ProposeBond}
  import BottomUpBondScout.{
    GoWithBottomUpBondScoutResponse,
    BondFromToSlipnetResponse,
    GoWithBottomUpBondScout2Response,
    BondFromTo2Response
  }
  import models.Workspace.GoWithBottomUpBondScout2

  var bondFrom: WorkspaceObjectRep = null
  var bondTo: WorkspaceObjectRep = null
  var bondFacet: SlipNodeRep = null
  var from_descriptor: Option[SlipNodeRep] = None
  var to_descriptor: Option[SlipNodeRep] = None
  var bondCategoryDegreeOfAssociation = 0.0
  var runTemperature: models.Coderack.Temperatures  = null

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, t) =>
      log.debug(s"BottomUpBondScout. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t

      workspace ! BondWithNeighbor(runTemperature)

    case GoWithBottomUpBondScoutResponse(from, to) =>
      log.debug(s"BottomUpBondScout. response")
      bondFrom = from
      bondTo = to
      // continue in slipnet with codelet.java.255
      slipnet ! BondFromTo(from, to)

    case BondFromToSlipnetResponse(fromFacets, toFacets) =>
      log.debug(s"BottomUpBondScout. BondFromToSlipnetResponse")
      workspace ! GoWithBottomUpBondScout2(bondFrom, bondTo, fromFacets, toFacets)

    case GoWithBottomUpBondScout2Response(bf, fd, td) =>
      bondFacet = bf
      from_descriptor = fd
      to_descriptor = td
      slipnet ! BondFromTo2(bondFrom, bondTo, from_descriptor, to_descriptor,bondFacet)

    case BondFromTo2Response(bondCategory: SlipNodeRep, bcda, slipnetLeft, slipnetRight) =>
      bondCategoryDegreeOfAssociation = bcda
      workspace ! WorkspaceProposeBond(
        bondFrom,
        bondTo,
        bondCategory,
        bondFacet,
        from_descriptor,
        to_descriptor,
        slipnetLeft,
        slipnetRight
      )

    case WorkspaceProposeBondResponse(bondID: String) =>
      coderack ! ProposeBond(bondID, bondCategoryDegreeOfAssociation)

    case Finished =>
      log.debug(s"BottomUpBondScout. Finished $runTemperature")
      workspace ! models.Workspace.Step(runTemperature)

  }


}


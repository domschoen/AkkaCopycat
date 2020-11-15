package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.SlipNode.SlipNodeRep
import models.Slipnet.{BondFromTo, BondFromTo2, WorkspaceStructureRep}
import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
import models.Workspace
import models.codelet.BottomUpBondScout.BondFromToSlipnetResponse
import models.codelet.Codelet.Finished

object BottomUpBondScout {
  case class GoWithBottomUpBondScoutResponse(from: WorkspaceStructureRep, to: WorkspaceStructureRep)
  case class BondFromToSlipnetResponse(fromFacets: List[SlipNodeRep], toFacets: List[SlipNodeRep])
  case class GoWithBottomUpBondScout2Response(bondFacet: SlipNodeRep, fromDescriptor: SlipNodeRep, toDescriptor: SlipNodeRep)
  case class BondFromTo2Response(bondCategory: SlipNodeRep,
                                 bondCategoryDegreeOfAssociation: Double,
                                 slipnetLeft: SlipNodeRep,
                                 slipnetRight: SlipNodeRep)
  case class GoWithBottomUpBondScout3Response(bondID: String)
}


// codelet.java.240
class BottomUpBondScout(urgency: Int,              workspace: ActorRef,
                        slipnet: ActorRef,
                        temperature: ActorRef,
                        arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.Run
  import Workspace.BondWithNeighbor
  import models.Coderack.{ChooseAndRun, ProposeBond}
  import BottomUpBondScout.{
    GoWithBottomUpBondScoutResponse,
    BondFromToSlipnetResponse,
    GoWithBottomUpBondScout2Response,
    BondFromTo2Response,
    GoWithBottomUpBondScout3Response
  }
  import models.Workspace.{GoWithBottomUpBondScout2, GoWithBottomUpBondScout3}

  var bondFrom: WorkspaceStructureRep = null
  var bondTo: WorkspaceStructureRep = null
  var bondFacet: SlipNodeRep = null
  var fromDescriptor: SlipNodeRep = null
  var toDescriptor: SlipNodeRep = null
  var bondCategoryDegreeOfAssociation = 0.0

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, runTemperature) =>
      log.debug(s"BottomUpBondScout. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)

      workspace ! BondWithNeighbor(runTemperature)

    case GoWithBottomUpBondScoutResponse(from, to) =>
      bondFrom = from
      bondTo = to
      // continue in slipnet with codelet.java.255
      slipnet ! BondFromTo(from, to)

    case BondFromToSlipnetResponse(fromFacets, toFacets) =>
      workspace ! GoWithBottomUpBondScout2(bondFrom, bondTo, fromFacets, toFacets)

    case GoWithBottomUpBondScout2Response(bf, fd, td) =>
      bondFacet = bf
      fromDescriptor = fd
      toDescriptor = td
      slipnet ! BondFromTo2(bondFrom, bondTo, fromDescriptor, toDescriptor)

    case BondFromTo2Response(bondCategory: SlipNodeRep, bcda, slipnetLeft, slipnetRight) =>
      bondCategoryDegreeOfAssociation = bcda
      workspace ! GoWithBottomUpBondScout3(
        bondFrom,
        bondTo,
        bondCategory,
        bondFacet,
        fromDescriptor,
        toDescriptor,
        bondCategoryDegreeOfAssociation,
        slipnetLeft,
        slipnetRight
      )

    case GoWithBottomUpBondScout3Response(bondID: String) =>
      coderack ! ProposeBond(bondID, bondCategoryDegreeOfAssociation)


    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      coderack ! ChooseAndRun

  }


}


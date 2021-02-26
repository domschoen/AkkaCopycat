package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.ProposeBond
import models.SlipNode.SlipNodeRep
import models.Workspace.{GoWithTopDownBondScout2, GoWithTopDownBondScoutCategory, GoWithTopDownBondScoutWithResponse, WorkspaceProposeBond, WorkspaceProposeBondResponse}
import models.WorkspaceObject.WorkspaceObjectRep

object TopDownBondScoutCategory {
  case class SlipnetTopDownBondScoutCategory2Response(isFromTo: Boolean,
                                                      urgency: Double,
                                                      bond_category: SlipNodeRep,
                                                      slipnetLeft: SlipNodeRep,
                                                      slipnetRight: SlipNodeRep
                                                     )

}
// Codelet.java.278
class TopDownBondScoutCategory(urgency: Int,
                               workspace: ActorRef,
                               slipnet: ActorRef,
                               arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import TopDownBondScoutCategory.{
    SlipnetTopDownBondScoutCategory2Response
  }
  import models.Slipnet.{SlipnetTopDownBondScoutResponse, GoWithTopDownBondScout2Response, SlipnetTopDownBondScout, SlipnetTopDownBondScoutCategory2}

  var bondFrom: WorkspaceObjectRep = null
  var bondTo: WorkspaceObjectRep = null
  var from_descriptor: Option[SlipNodeRep] = None
  var to_descriptor: Option[SlipNodeRep] = None
  var bond_facet: SlipNodeRep = null
  var bond_urgency: Double = 0.0
  var runTemperature: models.Coderack.Temperatures  = null

  def bondCategoryID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t
      workspace ! GoWithTopDownBondScoutCategory(bondCategoryID, t)

    case GoWithTopDownBondScoutWithResponse(from, to, fromdtypes, todtypes) =>
      bondFrom = from
      bondTo = to
      // continue in slipnet with codelet.java.255
      slipnet ! SlipnetTopDownBondScout(fromdtypes, todtypes)

    case SlipnetTopDownBondScoutResponse(bondfacets) =>
      workspace ! GoWithTopDownBondScout2(bondFrom, bondTo, bondfacets)


    case GoWithTopDownBondScout2Response(bf, from_d, to_d) =>
      from_descriptor = from_d
      to_descriptor = to_d
      bond_facet = bf
      slipnet ! SlipnetTopDownBondScoutCategory2(bondCategoryID, from_descriptor, to_descriptor, bond_facet)

    case SlipnetTopDownBondScoutCategory2Response(isFromTo, urgency, bond_category, slipnetLeft, slipnetRight) =>
      log.debug("SlipnetTopDownBondScoutCategory2Response isFromTo " + isFromTo)
      bond_urgency = urgency
      val bond1 = if (isFromTo)  bondFrom else bondTo
      val bond2 = if (isFromTo)  bondTo else bondFrom
      val descriptor1 = if (isFromTo)  from_descriptor else to_descriptor
      val descriptor2 = if (isFromTo)  to_descriptor else from_descriptor

      workspace ! WorkspaceProposeBond(
        bond1,
        bond2,
        bond_category,
        bond_facet,
        descriptor1,
        descriptor2,
        slipnetLeft,
        slipnetRight
      )

    case WorkspaceProposeBondResponse(bondID: String) =>
      coderack ! ProposeBond(bondID, bond_urgency)


    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


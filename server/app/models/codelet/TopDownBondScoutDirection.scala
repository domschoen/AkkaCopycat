package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.ProposeBond
import models.SlipNode.SlipNodeRep
import models.Slipnet.{GoWithTopDownBondScout2Response, SlipnetTopDownBondScout, SlipnetTopDownBondScoutCategory2, SlipnetTopDownBondScoutDirection2, SlipnetTopDownBondScoutResponse}
import models.Workspace.{GoWithTopDownBondScout2, GoWithTopDownBondScoutDirection, GoWithTopDownBondScoutWithResponse, WorkspaceProposeBond, WorkspaceProposeBondResponse}
import models.WorkspaceObject.WorkspaceObjectRep

object TopDownBondScoutDirection {

  case class SlipnetTopDownBondScoutDirection2Response(
                                                      urgency: Double,
                                                      bond_category: SlipNodeRep,
                                                      slipnetLeft: SlipNodeRep,
                                                      slipnetRight: SlipNodeRep
                                                     )

}

class TopDownBondScoutDirection(urgency: Int,
                                workspace: ActorRef,
                                slipnet: ActorRef,
                                temperature: ActorRef,
                                arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import TopDownBondScoutDirection.SlipnetTopDownBondScoutDirection2Response

  def directionID() = arguments.get.asInstanceOf[String]
  var bondFrom: WorkspaceObjectRep = null
  var bondTo: WorkspaceObjectRep = null
  var from_descriptor: Option[SlipNodeRep] = null
  var to_descriptor: Option[SlipNodeRep] = null
  var bond_facet: SlipNodeRep = null
  var bond_urgency: Double = 0.0
  var runTemperature = 0.0

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      workspace ! GoWithTopDownBondScoutDirection(directionID, t)

    case GoWithTopDownBondScoutWithResponse(from, to, fromdtypes, todtypes) =>
      bondFrom = from
      bondTo = to
      // continue in slipnet with codelet.java.255
      slipnet ! SlipnetTopDownBondScout(fromdtypes, todtypes)

    case SlipnetTopDownBondScoutResponse(fromdtypes, todtypes) =>
      workspace ! GoWithTopDownBondScout2(bondFrom, bondTo, todtypes)

    case GoWithTopDownBondScout2Response(bf, from_d, to_d) =>
      from_descriptor = from_d
      to_descriptor = to_d
      bond_facet = bf
      slipnet ! SlipnetTopDownBondScoutDirection2(directionID, from_descriptor, to_descriptor)

    case SlipnetTopDownBondScoutDirection2Response(urgency, bond_category, slipnetLeft, slipnetRight) =>
      bond_urgency = urgency
      workspace ! WorkspaceProposeBond(
        bondFrom,
        bondTo,
        bond_category,
        bond_facet,
        from_descriptor,
        to_descriptor,
        slipnetLeft,
        slipnetRight
      )
    case WorkspaceProposeBondResponse(bondID: String) =>
      coderack ! ProposeBond(bondID, bond_urgency)


    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Bond.BondRep
import models.Coderack.Step
import models.Correspondence.CorrespondenceRep
import models.Group.GroupRep
import models.Slipnet.{CorrespondenceUpdateStrengthData, PrepareBondFighting, PrepareBondFightingResponse, SlipnetGoWithBondStrengthTester}
import models.Workspace.{BondBuilderPostBondBreaking, BondBuilderPostCorrrespondencesBreaking, BondBuilderTryToBreakIncompatibleCorrespondences, GoWithBondBuilder, GoWithBondStrengthTester}
import models.codelet.BondBuilder.BondBuilderNoIncompatibleGroups


object BondBuilder {
  case class BondBuilderTryingToBreakIncompatibleBonds(incb: List[BondRep])
  case object BondBuilderNoIncompatibleBonds
  case object BondBuilderWonBondsFight

  case class BondBuilderTryingToBreakIncompatibleCorrespondences(incc: List[CorrespondenceRep], incg: List[GroupRep], workspaceCorrespondences: List[CorrespondenceRep])
  case class BondBuilderNoIncompatibleCorrespondences(incc: List[CorrespondenceRep], incg: List[GroupRep])

  case class BondBuilderTryingToBreakIncompatibleGroups(incg: List[GroupRep])

  case class PrepareCorrespondenceFightingResponse(cDatas: Map[String, CorrespondenceUpdateStrengthData])
  case object BondBuilderWonCorrespondencesFight

  case object BondBuilderNoIncompatibleGroups
  case class PrepareGroupFightingResponse(degOfs: Map[String, Double])
  case object BondBuilderWonGroupsFight

  case class SlipnetBondBuilderPrepareGroupFightingResponse(degOfs: Map[String, Double])
}
class BondBuilder(urgency: Int, workspace: ActorRef,
                  slipnet: ActorRef,
                  arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Workspace.{
    GoWithBondStrengthTester,
    GoWithBondStrengthTesterResponse,
    BondBuilderTryToBreakIncompatibleBonds,
    BondBuilderPostGroupBreaking,
    BondBuilderTryToBreakIncompatibleGroups
  }
  import models.Slipnet.{
    SlipnetGoWithBondStrengthTesterResponse,
    PrepareCorrespondenceFighting,
    BondBuilderPrepareGroupFighting

  }
  import BondBuilder.{
    BondBuilderTryingToBreakIncompatibleBonds,
    BondBuilderNoIncompatibleBonds,
    BondBuilderTryingToBreakIncompatibleCorrespondences,
    BondBuilderNoIncompatibleCorrespondences,
    PrepareCorrespondenceFightingResponse,
    BondBuilderWonCorrespondencesFight,
    BondBuilderWonBondsFight,
    BondBuilderTryingToBreakIncompatibleGroups,
    SlipnetBondBuilderPrepareGroupFightingResponse,
    BondBuilderWonGroupsFight
  }

  var runTemperature: models.Coderack.Temperatures  = null
  def bondID() = arguments.get.asInstanceOf[String]
  var bondRep: BondRep = null
  var incb: List[BondRep] = null
  var incg: List[GroupRep] = null
  var incc: List[CorrespondenceRep] = null
  var bond_category_degree_of_association: Double = 0.0

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, t) =>
      log.debug(s"BondBuilder. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t

      workspace ! GoWithBondStrengthTester(runTemperature, bondID)


    case GoWithBondStrengthTesterResponse(bRep) =>
      log.debug("GoWithBondStrengthTesterResponse")
      bondRep = bRep
      slipnet ! SlipnetGoWithBondStrengthTester(bondRep)


    case SlipnetGoWithBondStrengthTesterResponse(bcda) =>
      log.debug("BondBuilder. SlipnetGoWithBondStrengthTesterResponse")
      bond_category_degree_of_association = bcda
      workspace ! GoWithBondBuilder(runTemperature, bondID(), bond_category_degree_of_association)


      // Bonds
    case BondBuilderTryingToBreakIncompatibleBonds(incbReps) =>
      log.debug("BondBuilderTryingToBreakIncompatibleBonds " + incbReps)
      incb = incbReps
      val bondRepList = bondRep :: incb
      slipnet ! PrepareBondFighting(bondRepList)

    case PrepareBondFightingResponse(degOfAssos: Map[String, Double]) =>
      log.debug("PrepareBondFightingResponse")
      workspace ! BondBuilderTryToBreakIncompatibleBonds(bondID(),incb, degOfAssos,runTemperature)

    case BondBuilderNoIncompatibleGroups =>
      log.debug("BondBuilderNoIncompatibleGroups")
      incg = List.empty[GroupRep]

      workspace ! BondBuilderPostGroupBreaking(bondID(), incg)


    case BondBuilderNoIncompatibleBonds =>
      log.debug("BondBuilderNoIncompatibleBonds")
      incb = List.empty[BondRep]

      workspace ! BondBuilderPostBondBreaking(bondID(), bond_category_degree_of_association)

    case BondBuilderWonBondsFight =>
      log.debug("BondBuilderWonBondsFight")
      workspace ! BondBuilderPostBondBreaking(bondID(), bond_category_degree_of_association)


      // Groups
    case BondBuilderTryingToBreakIncompatibleGroups(incgReps) =>
      incg = incgReps
      slipnet ! BondBuilderPrepareGroupFighting(incg)

    case SlipnetBondBuilderPrepareGroupFightingResponse(degOfs) =>
      workspace ! BondBuilderTryToBreakIncompatibleGroups(bondID(),incg,bond_category_degree_of_association, degOfs, runTemperature)

    case BondBuilderWonGroupsFight =>
      workspace ! BondBuilderPostGroupBreaking(bondID(), incg)

    case BondBuilderTryingToBreakIncompatibleCorrespondences(inccReps,  incgReps, workspaceCorrespondences) =>
      log.debug("BondBuilderTryingToBreakIncompatibleCorrespondences")
      incc = inccReps
      incg = incgReps
      slipnet ! PrepareCorrespondenceFighting(incc, workspaceCorrespondences)

    case PrepareCorrespondenceFightingResponse(cDatas) =>
      log.debug("PrepareCorrespondenceFightingResponse")
      workspace ! BondBuilderTryToBreakIncompatibleCorrespondences(bondID, incc, bond_category_degree_of_association, cDatas, runTemperature)

    case BondBuilderWonCorrespondencesFight =>
      log.debug("BondBuilderWonCorrespondencesFight")
      workspace ! BondBuilderPostCorrrespondencesBreaking(bondID(), incb, incg, incc)


    case BondBuilderNoIncompatibleCorrespondences(inccReps, incgReps) =>
      log.debug("BondBuilderNoIncompatibleCorrespondences")
      incc = inccReps
      incg = incgReps
      workspace ! BondBuilderPostCorrrespondencesBreaking(bondID(), incb, incg, incc)


    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Bond.BondRep
import models.Coderack.Temperatures
import models.ConceptMapping.{ConceptMappingParameters, ConceptMappingRep}
import models.Slipnet.{CorrespondenceUpdateStrengthData, SlipnetCompleteSlippageList, SlipnetCompleteSlippageListResponse, SlipnetGoWithCorrespondenceBuilder, SlipnetGoWithCorrespondenceBuilder2, SlipnetGoWithCorrespondenceBuilder5, SlipnetGoWithCorrespondenceBuilder6, SlipnetGoWithGroupStrengthTester}
import models.Workspace.{GoWithCorrespondenceBuilder10Continue, GoWithCorrespondenceBuilder10Fight, GoWithCorrespondenceBuilder3, GoWithCorrespondenceBuilder4, GoWithCorrespondenceBuilder5, GoWithCorrespondenceBuilder8, GoWithCorrespondenceBuilder9Response, SlippageListShell}
import models.WorkspaceObject.WorkspaceObjectRep
import models.Correspondence.CorrespondenceRep
import models.Group.GroupRep
import models.SlipNode.SlipNodeRep
import models.codelet.CorrespondenceBuilder.SlipnetGoWithCorrespondenceBuilderResponse2
import models.codelet.GroupStrengthTester.SlipnetGoWithGroupStrengthTesterResponse

// Codelet.java.1476
object CorrespondenceBuilder {

  case class GoWithCorrespondenceBuilder2Response(conceptMappingReps: List[ConceptMappingRep], existingConceptMappingReps: List[ConceptMappingRep])
  case class GoWithCorrespondenceBuilder3Response(corrrespondence: CorrespondenceRep, correspondenceReps: List[CorrespondenceRep], wcorrespondenceReps: List[CorrespondenceRep])
  case class GoWithCorrespondenceBuilder7Response(groupObjs: Option[ConceptMappingParameters])
  case class GoWithCorrespondenceBuilder8Response(cms: List[ConceptMappingRep])
  case class SlipnetGoWithCorrespondenceBuilderResponse(updatedCorrespondenceCMReps: List[ConceptMappingRep])
  case class SlipnetGoWithCorrespondenceBuilderResponse2(
                                                          correspondenceReps: List[CorrespondenceRep],
                                                          cData: CorrespondenceUpdateStrengthData,
                                                          inccData:Map[String, CorrespondenceUpdateStrengthData]
                                                        )
  case class SlipnetGoWithCorrespondenceBuilderResponse3(b: Option[BondRep],
                                                         cData: CorrespondenceUpdateStrengthData,
                                                         bond_category_degree_of_associationOpt: Option[Double]
                                                        )
  case class GoWithCorrespondenceBuilder4Response1(corrrespondence: CorrespondenceRep, incompatible_bond_base: Option[(BondRep,BondRep)], wCReps: List[CorrespondenceRep])
  case class GoWithCorrespondenceBuilder4Response2(g: Option[GroupRep],cData: CorrespondenceUpdateStrengthData)
  case class GoWithCorrespondenceBuilder6Response(c: CorrespondenceRep)
  case class SlipnetGoWithCorrespondenceBuilder4Response(accessory_concept_mapping_list: List[ConceptMappingRep])
  case class SlipnetGoWithCorrespondenceBuilder5Response(accessory_concept_mapping_list: List[ConceptMappingRep])
  case class GoWithCorrespondenceBuilder6ResponseFight(ruleUUID: String, slippageListShell: SlippageListShell)
  case object GoWithCorrespondenceBuilder16ContinuePostFight

  case class CorrespondenceBuilderTryingToFightIncompatibleGroups(incGroup: GroupRep, cData: CorrespondenceUpdateStrengthData)
  case object CorrespondenceBuilderWonGroupsFight


}
class CorrespondenceBuilder(urgency: Int,
                            workspace: ActorRef,
                            slipnet: ActorRef,
                            arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Workspace.{
    GoWithCorrespondenceBuilder,
    GoWithCorrespondenceBuilderResponse,

    GoWithCorrespondenceBuilder2,
    GoWithCorrespondenceBuilder6,
    GoWithCorrespondenceBuilder7,
    GoWithCorrespondenceBuilder9,
    CorrespondenceBuilderTryToBreakIncompatibleGroups
  }
  import CorrespondenceBuilder.{
    GoWithCorrespondenceBuilder2Response,
    GoWithCorrespondenceBuilder3Response,
    SlipnetGoWithCorrespondenceBuilderResponse,
    GoWithCorrespondenceBuilder4Response1,
    GoWithCorrespondenceBuilder4Response2,
    SlipnetGoWithCorrespondenceBuilderResponse3,
    GoWithCorrespondenceBuilder6Response,
    SlipnetGoWithCorrespondenceBuilder4Response,
    GoWithCorrespondenceBuilder7Response,
    SlipnetGoWithCorrespondenceBuilder5Response,
    GoWithCorrespondenceBuilder8Response,
    GoWithCorrespondenceBuilder6ResponseFight,
    GoWithCorrespondenceBuilder16ContinuePostFight,
    CorrespondenceBuilderTryingToFightIncompatibleGroups,
    CorrespondenceBuilderWonGroupsFight
  }
  import models.Slipnet.{
    GroupFlippedVersion,
    GroupFlippedVersionResponse,
    SlipnetGoWithCorrespondenceBuilder3,
    SlipnetGoWithCorrespondenceBuilder4

  }

  var runTemperature: Temperatures = null

  def corresponsdenceID() = arguments.get.asInstanceOf[String]

  var incc: List[CorrespondenceRep] = null

  var incompatible_bond: Option[BondRep] = None

  var incompatible_group = Option.empty[GroupRep]

  var ruleUUIDOpt = Option.empty[String]
  var correspondenceUpdateStrengthData: CorrespondenceUpdateStrengthData = null

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, t) =>
      log.debug(s"CorrespondenceBuilder. Run with initial $initialString, modified: $modifiedString and target: $targetString")

      coderack = sender()
      runTemperature = t
      workspace ! GoWithCorrespondenceBuilder(runTemperature, corresponsdenceID)


    // Codelet.java.1478
    case GoWithCorrespondenceBuilderResponse(obj2) =>
      obj2.asGroupRep match {
        case Some(gr) =>
          log.debug("CorrespondenceBuilder: is a Group")
          slipnet ! GroupFlippedVersion(gr)
        case None =>
          log.debug("CorrespondenceBuilder: is a " + obj2.getClass)
          workspace ! GoWithCorrespondenceBuilder9(corresponsdenceID,runTemperature)
      }

    case GroupFlippedVersionResponse(fgr) =>
      if (fgr.isEmpty) {
        println("Stuffer: empty flipped. Fizzle")
        self ! Finished
      } else {
        workspace ! GoWithCorrespondenceBuilder2(corresponsdenceID, fgr.get, runTemperature)
      }

    // From FlipppedGroup
    case GoWithCorrespondenceBuilder9Response =>
      log.debug("GoWithCorrespondenceBuilder9Response")
      workspace ! GoWithCorrespondenceBuilder9(corresponsdenceID,runTemperature)


    //Codelet.java.1495
    case GoWithCorrespondenceBuilder2Response(conceptMappingReps: List[ConceptMappingRep], existingConceptMappingReps) =>
      slipnet ! SlipnetGoWithCorrespondenceBuilder(conceptMappingReps, existingConceptMappingReps)

    case SlipnetGoWithCorrespondenceBuilderResponse(updatedCorrespondenceCMReps) =>
      workspace ! GoWithCorrespondenceBuilder3(corresponsdenceID, updatedCorrespondenceCMReps)

    case GoWithCorrespondenceBuilder3Response(correspondence, correspondenceReps, wCReps) =>
      log.debug("CorrespondenceBuilder. GoWithCorrespondenceBuilder3Response")
      slipnet ! SlipnetGoWithCorrespondenceBuilder2(correspondence, correspondenceReps, wCReps)

    case SlipnetGoWithCorrespondenceBuilderResponse2(correspondenceReps, cData, inccData) =>
      log.debug("CorrespondenceBuilder. SlipnetGoWithCorrespondenceBuilderResponse2")
      incc = correspondenceReps
      workspace ! GoWithCorrespondenceBuilder4(corresponsdenceID, incc, cData, inccData,runTemperature)

    case GoWithCorrespondenceBuilder4Response1(correspondence, incompatible_bond_base, wCReps) =>
      log.debug("CorrespondenceBuilder. GoWithCorrespondenceBuilder4Response1")
      slipnet ! SlipnetGoWithCorrespondenceBuilder3(correspondence, incompatible_bond_base, wCReps)


    case  SlipnetGoWithCorrespondenceBuilderResponse3(bOpt, cData, bond_category_degree_of_associationOpt) =>
      log.debug("CorrespondenceBuilder. SlipnetGoWithCorrespondenceBuilderResponse3")
      incompatible_bond = bOpt
      workspace ! GoWithCorrespondenceBuilder5(corresponsdenceID, bOpt,
        cData,
        bond_category_degree_of_associationOpt,
        runTemperature
      )



      //2 branches joining here
    case GoWithCorrespondenceBuilder4Response2(g, cData) =>
      correspondenceUpdateStrengthData = cData
      log.debug("CorrespondenceBuilder. GoWithCorrespondenceBuilder4Response2")
      incompatible_group = g
      workspace ! GoWithCorrespondenceBuilder6(corresponsdenceID)

    case GoWithCorrespondenceBuilder6Response(crep) =>
      log.debug("CorrespondenceBuilder. GoWithCorrespondenceBuilder6Response")
      slipnet ! SlipnetGoWithCorrespondenceBuilder4(crep)

    case SlipnetGoWithCorrespondenceBuilder4Response(accessory_concept_mapping_list) =>
      log.debug("CorrespondenceBuilder. SlipnetGoWithCorrespondenceBuilder4Response")
      workspace ! GoWithCorrespondenceBuilder7(corresponsdenceID, accessory_concept_mapping_list)

    case GoWithCorrespondenceBuilder6ResponseFight(ruleUUID: String, slippageListShell) =>
      log.debug("GoWithCorrespondenceBuilder6ResponseFight")
      ruleUUIDOpt = Some(ruleUUID)
      slipnet ! SlipnetCompleteSlippageList(slippageListShell)

    case SlipnetCompleteSlippageListResponse(slippage_list_rep) =>
      log.debug("SlipnetCompleteSlippageListResponse")
      workspace ! GoWithCorrespondenceBuilder10Fight(corresponsdenceID, correspondenceUpdateStrengthData, slippage_list_rep, runTemperature)

    case CorrespondenceBuilderTryingToFightIncompatibleGroups(incGroup, cData) =>
      log.debug("CorrespondenceBuilderTryingToFightIncompatibleGroups")
      incompatible_group = Some(incGroup)
      correspondenceUpdateStrengthData = cData
      slipnet ! SlipnetGoWithGroupStrengthTester(incGroup.group_category.id)

    case SlipnetGoWithGroupStrengthTesterResponse(degree_of_association) =>
      log.debug("SlipnetGoWithGroupStrengthTesterResponse " + corresponsdenceID + " " + incompatible_group.get + " " + correspondenceUpdateStrengthData + " " +degree_of_association)
      workspace ! CorrespondenceBuilderTryToBreakIncompatibleGroups(
        corresponsdenceID,
        incompatible_group.get,
        correspondenceUpdateStrengthData,
        degree_of_association, runTemperature)

    case CorrespondenceBuilderWonGroupsFight =>
      workspace ! GoWithCorrespondenceBuilder6(corresponsdenceID)


    case GoWithCorrespondenceBuilder16ContinuePostFight =>
      log.debug("GoWithCorrespondenceBuilder16ContinuePostFight")
      workspace ! GoWithCorrespondenceBuilder10Continue(corresponsdenceID, incc, incompatible_bond, incompatible_group, ruleUUIDOpt)


    case   GoWithCorrespondenceBuilder7Response(groupObjs) =>
      log.debug("GoWithCorrespondenceBuilder7Response")
      slipnet ! SlipnetGoWithCorrespondenceBuilder5(groupObjs)

    case SlipnetGoWithCorrespondenceBuilder5Response(accessory_concept_mapping_list) =>
      log.debug("SlipnetGoWithCorrespondenceBuilder5Response")
      log.debug("Correspondence accessory_concept_mapping_list " + corresponsdenceID +  accessory_concept_mapping_list);

      workspace ! GoWithCorrespondenceBuilder8(corresponsdenceID, accessory_concept_mapping_list)

    case GoWithCorrespondenceBuilder8Response(cms) =>
      log.debug("GoWithCorrespondenceBuilder8Response")
      slipnet ! SlipnetGoWithCorrespondenceBuilder6(cms)


    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)


  }


}


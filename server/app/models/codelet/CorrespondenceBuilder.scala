package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Bond.BondRep
import models.ConceptMapping.{ConceptMappingParameters, ConceptMappingRep}
import models.Slipnet.{SlipnetGoWithCorrespondenceBuilder, SlipnetGoWithCorrespondenceBuilder2, SlipnetGoWithCorrespondenceBuilder5, SlipnetGoWithCorrespondenceBuilder6}
import models.Workspace.{GoWithCorrespondenceBuilder3, GoWithCorrespondenceBuilder4, GoWithCorrespondenceBuilder5, GoWithCorrespondenceBuilder8}
import models.WorkspaceObject.WorkspaceObjectRep
import models.Correspondence.CorrespondenceRep
import models.Group.GroupRep
import models.codelet.CorrespondenceBuilder.{GoWithCorrespondenceBuilder9Response, SlipnetGoWithCorrespondenceBuilderResponse2}

// Codelet.java.1476
object CorrespondenceBuilder {
  case object GoWithCorrespondenceBuilder9Response

  case class GoWithCorrespondenceBuilder2Response(conceptMappingReps: List[ConceptMappingRep])
  case class GoWithCorrespondenceBuilder3Response(corrrespondence: CorrespondenceRep, correspondenceReps: List[CorrespondenceRep])
  case class GoWithCorrespondenceBuilder7Response(groupObjs: Option[ConceptMappingParameters])
  case class GoWithCorrespondenceBuilder8Response(cms: List[ConceptMappingRep])
  case class SlipnetGoWithCorrespondenceBuilderResponse(updatedCorrespondenceCMReps: List[ConceptMappingRep])
  case class SlipnetGoWithCorrespondenceBuilderResponse2(correspondenceReps: List[CorrespondenceRep])
  case class SlipnetGoWithCorrespondenceBuilderResponse3(b: Option[BondRep])
  case class GoWithCorrespondenceBuilder4Response1(corrrespondence: CorrespondenceRep, incompatible_bond_base: Option[(BondRep,BondRep)])
  case class GoWithCorrespondenceBuilder4Response2(g: Option[GroupRep])
  case class GoWithCorrespondenceBuilder6Response(c: CorrespondenceRep)
  case class SlipnetGoWithCorrespondenceBuilder4Response(accessory_concept_mapping_list: List[ConceptMappingRep])
  case class SlipnetGoWithCorrespondenceBuilder5Response(accessory_concept_mapping_list: List[ConceptMappingRep])
}
class CorrespondenceBuilder(urgency: Int,
                            workspace: ActorRef,
                            slipnet: ActorRef,
                            temperature: ActorRef,
                            arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import models.Workspace.{
    GoWithCorrespondenceBuilder,
    GoWithCorrespondenceBuilderResponse,

    GoWithCorrespondenceBuilder2,
    GoWithCorrespondenceBuilder6,
    GoWithCorrespondenceBuilder7,
    GoWithCorrespondenceBuilder9
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
    GoWithCorrespondenceBuilder8Response
  }
  import models.Slipnet.{
    GroupFlippedVersion,
    GroupFlippedVersionResponse,
    SlipnetGoWithCorrespondenceBuilder3,
    SlipnetGoWithCorrespondenceBuilder4

  }

  var runTemperature: Double = 0.0

  def corresponsdenceID() = arguments.get.asInstanceOf[String]

  var incc: List[CorrespondenceRep] = null

  var incompatible_bond: Option[BondRep] = None

  var incompatible_group = Option.empty[GroupRep]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, t) =>
      log.debug(s"CorrespondenceBuilder. Run with initial $initialString, modified: $modifiedString and target: $targetString")

      coderack = sender()
      temperature ! Register(self)
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
      workspace ! GoWithCorrespondenceBuilder9(corresponsdenceID,runTemperature)


    //Codelet.java.1495
    case GoWithCorrespondenceBuilder2Response(conceptMappingReps: List[ConceptMappingRep]) =>
      slipnet ! SlipnetGoWithCorrespondenceBuilder(conceptMappingReps)

    case SlipnetGoWithCorrespondenceBuilderResponse(updatedCorrespondenceCMReps) =>
      workspace ! GoWithCorrespondenceBuilder3(corresponsdenceID, updatedCorrespondenceCMReps)

    case GoWithCorrespondenceBuilder3Response(correspondence, correspondenceReps) =>
      log.debug("CorrespondenceBuilder. GoWithCorrespondenceBuilder3Response")
      slipnet ! SlipnetGoWithCorrespondenceBuilder2(correspondence, correspondenceReps)

    case SlipnetGoWithCorrespondenceBuilderResponse2(correspondenceReps) =>
      log.debug("CorrespondenceBuilder. SlipnetGoWithCorrespondenceBuilderResponse2")
      incc = correspondenceReps
      workspace ! GoWithCorrespondenceBuilder4(corresponsdenceID, incc)

    case GoWithCorrespondenceBuilder4Response1(correspondence, incompatible_bond_base) =>
      log.debug("CorrespondenceBuilder. GoWithCorrespondenceBuilder4Response1")
      slipnet ! SlipnetGoWithCorrespondenceBuilder3(correspondence, incompatible_bond_base)


    case  SlipnetGoWithCorrespondenceBuilderResponse3(bOpt) =>
      incompatible_bond = bOpt
      workspace ! GoWithCorrespondenceBuilder5(corresponsdenceID, bOpt)

      //2 branches joining here
    case GoWithCorrespondenceBuilder4Response2(g) =>
      incompatible_group = g
      workspace ! GoWithCorrespondenceBuilder6(corresponsdenceID, incc, incompatible_bond, incompatible_group)

    case GoWithCorrespondenceBuilder6Response(crep) =>
      slipnet ! SlipnetGoWithCorrespondenceBuilder4(crep)

    case SlipnetGoWithCorrespondenceBuilder4Response(accessory_concept_mapping_list) =>
      workspace ! GoWithCorrespondenceBuilder7(corresponsdenceID, accessory_concept_mapping_list)

    case GoWithCorrespondenceBuilder7Response(groupObjs) =>
      slipnet ! SlipnetGoWithCorrespondenceBuilder5(groupObjs)

    case SlipnetGoWithCorrespondenceBuilder5Response(accessory_concept_mapping_list) =>
      workspace ! GoWithCorrespondenceBuilder8(corresponsdenceID, accessory_concept_mapping_list)

    case GoWithCorrespondenceBuilder8Response(cms) =>
      slipnet ! SlipnetGoWithCorrespondenceBuilder6(cms)


    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)


  }


}


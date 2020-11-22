package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Coderack.ChooseAndRun
import models.ConceptMapping.ConceptMappingRep
import models.Slipnet.{ProposeAnyCorrespondence, WorkspaceStructureRep}
import models.Workspace.GoWithBottomUpCorrespondenceScout2
import models.codelet.BottomUpCorrespondenceScout.GoWithBottomUpCorrespondenceScout2Response
import models.codelet.Codelet.Finished

object BottomUpCorrespondenceScout{

  case class GoWithBottomUpCorrespondenceScoutWorkspaceReponse(
                                     obj1 :WorkspaceStructureRep,
                                     obj2: WorkspaceStructureRep
                                   )
  case class ProposeAnyCorrespondenceSlipnetResponse(
                                                      obj1: WorkspaceStructureRep,
                                                      obj2: WorkspaceStructureRep,
                                                      concept_mapping_list : List[ConceptMappingRep],
                                                      flip_obj2: Boolean,
                                                      distiguishingConceptMappingSize: Int,
                                                      distiguishingConceptMappingTotalStrength: Double
                                                    )
  case class GoWithBottomUpCorrespondenceScout2Response(
                                                         correspondenceID: String,
                                                         distiguishingConceptMappingSize: Int,
                                                         distiguishingConceptMappingTotalStrength: Double
                                                       )

}


// codelet.java.1233
class BottomUpCorrespondenceScout(urgency: Int,
                                  workspace: ActorRef,
                                  slipnet: ActorRef,
                                  temperature: ActorRef,
                                  arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Workspace.GoWithBottomUpCorrespondenceScout
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import BottomUpCorrespondenceScout.GoWithBottomUpCorrespondenceScoutWorkspaceReponse
  import BottomUpCorrespondenceScout.ProposeAnyCorrespondenceSlipnetResponse
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, runTemperature) =>
      log.debug(s"BottomUpCorrespondenceScout. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)

      workspace ! GoWithBottomUpCorrespondenceScout(runTemperature)


    case GoWithBottomUpCorrespondenceScoutWorkspaceReponse(
      obj1 :WorkspaceStructureRep,
      obj2: WorkspaceStructureRep
    ) =>
      slipnet ! ProposeAnyCorrespondence(
        obj1,
        obj2,
        t
      )

    case ProposeAnyCorrespondenceSlipnetResponse(
                                                     obj1,
                                                     obj2,
                                                     concept_mapping_list,
                                                     flip_obj2,
                                                     distiguishingConceptMappingSize,
                                                     distiguishingConceptMappingTotalStrength
                                                   ) =>
      workspace !  GoWithBottomUpCorrespondenceScout2(
        obj1,
        obj2,
        concept_mapping_list,
        flip_obj2,
        distiguishingConceptMappingSize,
        distiguishingConceptMappingTotalStrength,
        t
      )

    case GoWithBottomUpCorrespondenceScout2Response(correspondenceID: String,
      distiguishingConceptMappingSize: Int,
      distiguishingConceptMappingTotalStrength) =>
      coderack ! ProposeCorrespondence(correspondenceID,distiguishingConceptMappingSize,distiguishingConceptMappingTotalStrength)


    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      coderack ! ChooseAndRun

  }


}


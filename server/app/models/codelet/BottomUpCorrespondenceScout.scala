package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Coderack.ChooseAndRun
import models.ConceptMapping.ConceptMappingRep
import models.Group.FutureGroupRep
import models.Workspace.{GoWithBottomUpCorrespondenceScout2, GoWithBottomUpCorrespondenceScout3}
import models.WorkspaceObject.WorkspaceObjectRep
import models.codelet.BottomUpCorrespondenceScout.{GoWithBottomUpCorrespondenceScout2Response, GoWithBottomUpCorrespondenceScout3Response, ProposeAnyCorrespondenceSlipnetResponse2}
import models.codelet.Codelet.Finished

object BottomUpCorrespondenceScout{

  case class GoWithBottomUpCorrespondenceScoutWorkspaceReponse(
                                                                obj1 :WorkspaceObjectRep,
                                                                obj2: WorkspaceObjectRep
                                   )
  case class GoWithBottomUpCorrespondenceScout3Response(newObj2: WorkspaceObjectRep)
  case class ProposeAnyCorrespondenceSlipnetResponse(fg: FutureGroupRep)

  case class ProposeAnyCorrespondenceSlipnetResponse2(
                                                      obj1: WorkspaceObjectRep,
                                                      obj2: WorkspaceObjectRep,
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
  import models.Slipnet.{
    SlipnetBottomUpCorrespondenceScout,
    SlipnetBottomUpCorrespondenceScout2
  }
  var obj1 :WorkspaceObjectRep = null
  var obj2: WorkspaceObjectRep = null
  var flip_obj2: Boolean = false

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, runTemperature) =>
      log.debug(s"BottomUpCorrespondenceScout. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)

      workspace ! GoWithBottomUpCorrespondenceScout(runTemperature)


    case GoWithBottomUpCorrespondenceScoutWorkspaceReponse(o1 :WorkspaceObjectRep, o2: WorkspaceObjectRep) =>
      obj1 = o1
      obj2 = o2
      slipnet ! SlipnetBottomUpCorrespondenceScout(
        obj1,
        obj2,
        t
      )

      // flipped case
    case ProposeAnyCorrespondenceSlipnetResponse(fg) =>
      workspace ! GoWithBottomUpCorrespondenceScout3(fg, obj2)

      // flipped case
    case GoWithBottomUpCorrespondenceScout3Response(newObj2) =>
      slipnet ! SlipnetBottomUpCorrespondenceScout2(obj1, newObj2)

    case ProposeAnyCorrespondenceSlipnetResponse2(
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
      workspace ! models.Workspace.Step

  }


}


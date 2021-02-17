package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Coderack.ChooseAndRun
import models.ConceptMapping.ConceptMappingRep
import models.Group.{GroupRep, FutureGroupRep}
import models.Slipnet.ProposeAnyCorrespondenceSlipnetResponse2
import models.Workspace.{GoWithBottomUpCorrespondenceScout2, GoWithBottomUpCorrespondenceScout2Response, GoWithBottomUpCorrespondenceScout3, GoWithBottomUpCorrespondenceScout3Response}
import models.WorkspaceObject.WorkspaceObjectRep
import models.codelet.Codelet.Finished

object BottomUpCorrespondenceScout{

  case class GoWithBottomUpCorrespondenceScoutWorkspaceReponse(
                                                                obj1 :WorkspaceObjectRep,
                                                                obj2: WorkspaceObjectRep
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
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import models.Slipnet.{
    SlipnetBottomUpCorrespondenceScout,
    SlipnetBottomUpCorrespondenceScout2,
    ProposeAnyCorrespondenceSlipnetResponse
  }
  var obj1 :WorkspaceObjectRep = null
  var obj2: WorkspaceObjectRep = null
  var flip_obj2: Boolean = false
  var runTemperature = 0.0

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, t) =>
      log.debug(s"BottomUpCorrespondenceScout. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      workspace ! GoWithBottomUpCorrespondenceScout(runTemperature)


    case GoWithBottomUpCorrespondenceScoutWorkspaceReponse(o1 :WorkspaceObjectRep, o2: WorkspaceObjectRep) =>
      obj1 = o1
      obj2 = o2
      slipnet ! SlipnetBottomUpCorrespondenceScout(
        obj1,
        obj2,
        runTemperature
      )

      // flipped case
    case ProposeAnyCorrespondenceSlipnetResponse(fg) =>
      workspace ! GoWithBottomUpCorrespondenceScout3(fg, obj2,runTemperature)

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
        runTemperature
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
      workspace ! models.Workspace.Step(runTemperature)

  }


}


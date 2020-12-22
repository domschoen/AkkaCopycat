package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.ConceptMapping.ConceptMappingRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.{ProposeAnyCorrespondenceSlipnetResponse2, SlipnetBottomUpCorrespondenceScout2, SlipnetGoWithImportantObjectCorrespondenceScout2}
import models.Workspace.{GoWithBottomUpCorrespondenceScout2, GoWithBottomUpCorrespondenceScout2Response, GoWithBottomUpCorrespondenceScout3, GoWithBottomUpCorrespondenceScout3Response, SlippageListShell}
import models.WorkspaceObject.WorkspaceObjectRep

// Codelet.java.1322
object ImportantObjectCorrespondenceScout {
  case class GoWithImportantObjectCorrespondenceScoutResponse(obj1: WorkspaceObjectRep, relevantDescriptors: List[SlipNodeRep])
  case class SlipnetGoWithImportantObjectCorrespondenceScoutResponse(s: Option[SlipNodeRep])
  case class GoWithImportantObjectCorrespondenceScout2Response(slippageListShell: SlippageListShell)
  case class GoWithImportantObjectCorrespondenceScout3Response(obj2: WorkspaceObjectRep)
}

class ImportantObjectCorrespondenceScout(urgency: Int,
                                         workspace: ActorRef,
                                         slipnet: ActorRef,
                                         temperature: ActorRef,
                                         arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import models.Workspace.{
    GoWithImportantObjectCorrespondenceScout,
    GoWithImportantObjectCorrespondenceScout2,
    GoWithImportantObjectCorrespondenceScout3
  }
  import ImportantObjectCorrespondenceScout.{
    GoWithImportantObjectCorrespondenceScoutResponse,
    SlipnetGoWithImportantObjectCorrespondenceScoutResponse,
    GoWithImportantObjectCorrespondenceScout2Response,
    GoWithImportantObjectCorrespondenceScout3Response
  }
  import models.Slipnet.{
    SlipnetGoWithImportantObjectCorrespondenceScout,
    SlipnetCompleteSlippageList,
    SlipnetCompleteSlippageListResponse,
    ProposeAnyCorrespondenceSlipnetResponse
  }


  var runTemperature: Double = 0.0
  var slipNode : SlipNodeRep = null
  var obj1: WorkspaceObjectRep = null
  var obj2: WorkspaceObjectRep = null

  def ruleID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      workspace ! GoWithImportantObjectCorrespondenceScout(t)



    case GoWithImportantObjectCorrespondenceScoutResponse(o1, relevantDescriptors) =>
      obj1 = o1
      slipnet ! SlipnetGoWithImportantObjectCorrespondenceScout(relevantDescriptors, runTemperature)

      // Codelet.java.1328
    case SlipnetGoWithImportantObjectCorrespondenceScoutResponse(sn) =>
      if (sn.isEmpty) {
        println("no relevant distinguishing descriptors found: fizzle");
        self ! Finished
      } else {
        slipNode = sn.get
        workspace ! GoWithImportantObjectCorrespondenceScout2
      }

      // Codelet.java.1333
    case GoWithImportantObjectCorrespondenceScout2Response(slippageListShell) =>
      slipnet ! SlipnetCompleteSlippageList(slippageListShell)

    // Codelet.java.1333
    case SlipnetCompleteSlippageListResponse(slippage_list_rep: List[ConceptMappingRep]) =>
      workspace ! GoWithImportantObjectCorrespondenceScout3(slippage_list_rep, slipNode, runTemperature, obj1)

    // Codelet.java.1369
    case GoWithImportantObjectCorrespondenceScout3Response(o2) =>
      obj2 = o2
      slipnet ! SlipnetGoWithImportantObjectCorrespondenceScout2(obj1, obj2, runTemperature)

    // flipped case
    case ProposeAnyCorrespondenceSlipnetResponse(fg) =>
      workspace ! GoWithBottomUpCorrespondenceScout3(fg, obj2, runTemperature)

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
      workspace ! models.Workspace.Step(runTemperature)

  }

}


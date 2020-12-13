package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.ConceptMapping.ConceptMappingRep
import models.SlipNode.SlipNodeRep
import models.Workspace.SlippageListShell
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
    SlipnetCompleteSlippageListResponse
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

    case SlipnetGoWithImportantObjectCorrespondenceScoutResponse(sn) =>
      if (sn.isEmpty) {
        println("no relevant distinguishing descriptors found: fizzle");
        self ! Finished
      } else {
        slipNode = sn.get
        workspace ! GoWithImportantObjectCorrespondenceScout2
      }

    case GoWithImportantObjectCorrespondenceScout2Response(slippageListShell) =>
      slipnet ! SlipnetCompleteSlippageList(slippageListShell)

    case SlipnetCompleteSlippageListResponse(slippage_list_rep: List[ConceptMappingRep]) =>
      workspace ! GoWithImportantObjectCorrespondenceScout3(slippage_list_rep, slipNode, runTemperature, obj1)

    case GoWithImportantObjectCorrespondenceScout3Response(o2) =>
      obj2 = o2
      // todo : continue with slipnet call

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step

  }

}


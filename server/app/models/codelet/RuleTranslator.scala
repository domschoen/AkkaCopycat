package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.Temperatures
import models.ConceptMapping.ConceptMappingRep
import models.Workspace.{GoWithRuleTranslator2, SlippageListShell}

// Codelet.java.1187
// TODO : see Workspace, case GoWithRuleTranslator2, "How to do that"
object RuleTranslator {
  case class GoWithRuleTranslatorResponse(slippageListShell: SlippageListShell)
  case object GoWithRuleTranslator2Response
}
class RuleTranslator(urgency: Int,
                     workspace: ActorRef,
                     slipnet: ActorRef,
                     arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Workspace.GoWithRuleTranslator
  import RuleTranslator.{
    GoWithRuleTranslatorResponse,
    GoWithRuleTranslator2Response
  }
  import models.Slipnet.{
    SlipnetCompleteSlippageList,
    SlipnetCompleteSlippageListResponse
  }

  var runTemperature: Temperatures = null
  def ruleID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t
      workspace ! GoWithRuleTranslator(runTemperature)


    case GoWithRuleTranslatorResponse(slippagesShell) =>
      slipnet ! SlipnetCompleteSlippageList(slippagesShell)

    case SlipnetCompleteSlippageListResponse(slippage_list_rep: List[ConceptMappingRep]) =>
      workspace ! GoWithRuleTranslator2(slippage_list_rep)

    case GoWithRuleTranslator2Response =>
      coderack ! models.Coderack.RuleTranslatorTemperatureUpdate(runTemperature)

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }

}


package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.ConceptMapping.ConceptMappingRep
import models.Workspace.{GoWithRuleTranslator2, SlippageListShell}

// Codelet.java.1187
// TODO : see Workspace, case GoWithRuleTranslator2, "How to do that"
object RuleTranslator {
  case class GoWithRuleTranslatorResponse(slippageListShell: SlippageListShell)
}
class RuleTranslator(urgency: Int,
                     workspace: ActorRef,
                     slipnet: ActorRef,
                     temperature: ActorRef,
                     arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import models.Workspace.GoWithRuleTranslator
  import RuleTranslator.GoWithRuleTranslatorResponse
  import models.Slipnet.{
    SlipnetCompleteSlippageList,
    SlipnetCompleteSlippageListResponse
  }

  var runTemperature: Double = 0.0
  def ruleID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      workspace ! GoWithRuleTranslator(runTemperature)


    case GoWithRuleTranslatorResponse(slippagesShell) =>
      slipnet ! SlipnetCompleteSlippageList(slippagesShell)

    case SlipnetCompleteSlippageListResponse(slippage_list_rep: List[ConceptMappingRep]) =>
      workspace ! GoWithRuleTranslator2(slippage_list_rep)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }

}


package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Workspace.SlippageListShell
import models.codelet.ImportantObjectCorrespondenceScout.GoWithImportantObjectCorrespondenceScout2Response

// Codelet.java.1151
object RuleBuilder {
  case class GoWithRulBuilderResponse(shell: SlippageListShell)
}
class RuleBuilder(urgency: Int,
                  workspace: ActorRef,
                  slipnet: ActorRef,
                  temperature: ActorRef,
                  arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import RuleBuilder.GoWithRulBuilderResponse
  import models.Slipnet.{
    SlipnetCompleteSlippageList,
    SlipnetCompleteSlippageListResponse
  }
  import models.Workspace.{
    GoWithRuleBuilder,
    GoWithRuleBuilder2
  }

  var runTemperature: Double = 0.0
  def ruleID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      workspace ! GoWithRuleBuilder(ruleID)

    case GoWithRulBuilderResponse(shell) =>
      slipnet ! SlipnetCompleteSlippageList(shell)

    case SlipnetCompleteSlippageListResponse(slippage_list_rep) =>
      workspace ! GoWithRuleBuilder2(ruleID, slippage_list_rep)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }

}


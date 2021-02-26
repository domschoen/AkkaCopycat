package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.{PostRuleBuilder, Temperatures}
import models.Slipnet.SlipnetCompleteSlippageList
import models.Workspace.SlippageListShell
import models.WorkspaceStructure.WorkspaceStructureRep

object RuleStrengthTester {
  case class GoWithRuleStrengthTesterResponse(slippageListShell: SlippageListShell)
  case class GoWithRuleStrengthTesterResponse2(rule: WorkspaceStructureRep, strength: Double)
}
class RuleStrengthTester(urgency: Int,
                         workspace: ActorRef,
                         slipnet: ActorRef,
                         arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Workspace.{
    GoWithRuleStrengthTester,
    GoWithRuleStrengthTester2
  }
  import models.Slipnet.SlipnetCompleteSlippageListResponse
  import RuleStrengthTester.{
    GoWithRuleStrengthTesterResponse,
    GoWithRuleStrengthTesterResponse2
  }
  var runTemperature: Temperatures = null
  var strength : Double = 0.0

  def ruleID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"RuleStrengthTester. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t
      workspace ! GoWithRuleStrengthTester

    case GoWithRuleStrengthTesterResponse(slippageListShell) =>
      slipnet ! SlipnetCompleteSlippageList(slippageListShell)

    case SlipnetCompleteSlippageListResponse(slippage_list_rep) =>
      workspace ! GoWithRuleStrengthTester2(runTemperature, ruleID, slippage_list_rep)

    case GoWithRuleStrengthTesterResponse2(rule, strength) =>
      coderack ! PostRuleBuilder(ruleID(),strength)


    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


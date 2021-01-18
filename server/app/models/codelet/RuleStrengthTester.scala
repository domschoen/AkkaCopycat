package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.PostRuleBuilder
import models.WorkspaceStructure.WorkspaceStructureRep

object RuleStrengthTester {
  case class GoWithRuleStrengthTesterResponse(rule: WorkspaceStructureRep, strength: Double)
}
class RuleStrengthTester(urgency: Int,
                         workspace: ActorRef,
                         slipnet: ActorRef,
                         temperature: ActorRef,
                         arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import models.Workspace.GoWithRuleStrengthTester
  import RuleStrengthTester.GoWithRuleStrengthTesterResponse
  var runTemperature: Double = 0.0
  var strength : Double = 0.0

  def ruleID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"RuleStrengthTester. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      workspace ! GoWithRuleStrengthTester(runTemperature, ruleID)

    case GoWithRuleStrengthTesterResponse(rule, strength) =>
      coderack ! PostRuleBuilder(ruleID(),strength)


    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


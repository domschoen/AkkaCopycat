package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.ProposeRule
import models.SlipNode.SlipNodeRep
import models.Slipnet.SlipnetProposeRuleResponse
import models.Workspace.{GoWithRuleScout, GoWithRuleScout3, WorkspaceProposeRuleResponse}
import models.WorkspaceObject.WorkspaceObjectRep

object RuleScout {
  case class RuleScoutProposeRule(descriptorFacet: Option[String],
                                  descriptor: Option[String],
                                  objectCategory: Option[String],
                                  relation: Option[String])
  case class GoWithRuleScoutResponse(changed: WorkspaceObjectRep)
  case class GoWithRuleScout2Response(obj2: WorkspaceObjectRep)
  case class SlipnetGoWithRuleScoutResponse(string_position_category: SlipNodeRep, letter_category: SlipNodeRep)
  case object SlipnetGoWithRuleScout2Response
}
class RuleScout(urgency: Int,
                workspace: ActorRef,
                slipnet: ActorRef,
                temperature: ActorRef,
                arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import RuleScout.{
    RuleScoutProposeRule,
    GoWithRuleScoutResponse,
    GoWithRuleScout2Response,
    SlipnetGoWithRuleScoutResponse,
    SlipnetGoWithRuleScout2Response
  }
  import models.Slipnet.{
    SlipnetProposeRule,
    SlipnetGoWithRuleScout,
    SlipnetGoWithRuleScout2
  }
  import models.Workspace.{
    WorkspaceProposeRule,
    GoWithRuleScout2
  }

  var runTemperature: Double = 0.0
  var descriptorFacet: Option[String] = None
  var descriptor: Option[String] = None
  var objectCategory: Option[String] = None
  var relation: Option[String] = None
  var u: Double = 0.0
  var changed : WorkspaceObjectRep = null

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t

      workspace ! GoWithRuleScout

    case RuleScoutProposeRule(df, d, oc, r) =>
      descriptorFacet = df
      descriptor = d
      objectCategory = oc
      relation = r

      slipnet ! SlipnetProposeRule(descriptor, relation)

    case SlipnetProposeRuleResponse(urg, lengthSlipNode, predecessorSlipNode, successorSlipNode) =>
      u = urg
      workspace ! WorkspaceProposeRule(descriptorFacet, descriptor,
        objectCategory, relation, lengthSlipNode,predecessorSlipNode, successorSlipNode)

    case WorkspaceProposeRuleResponse(ruleID) =>
      coderack ! ProposeRule(ruleID,u)

    case GoWithRuleScoutResponse(c) =>
      changed = c
      slipnet ! SlipnetGoWithRuleScout

    case SlipnetGoWithRuleScoutResponse(string_position_category, letter_category) =>
      workspace ! GoWithRuleScout2(changed, string_position_category, letter_category)


    case GoWithRuleScout2Response(obj2) =>
      slipnet ! SlipnetGoWithRuleScout2(obj2)

    case SlipnetGoWithRuleScout2Response =>
      workspace ! GoWithRuleScout3

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step

  }


}


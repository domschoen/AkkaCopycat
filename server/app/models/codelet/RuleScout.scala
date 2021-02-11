package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.ProposeRule
import models.ConceptMapping.ConceptMappingRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.{SlipnetCompleteSlippageList, SlipnetCompleteSlippageListResponse, SlipnetProposeRuleResponse}
import models.Workspace.{GoWithRuleScout, GoWithRuleScout3, SlippageListShell, WorkspaceProposeRuleResponse}
import models.WorkspaceObject.WorkspaceObjectRep
import models.WorkspaceStructure.WorkspaceStructureRep

object RuleScout {
  case class RuleScoutProposeRule(descriptorFacet: Option[SlipNodeRep],
                                  descriptor: Option[SlipNodeRep],
                                  objectCategory: Option[SlipNodeRep],
                                  relation: Option[SlipNodeRep])
  case class GoWithRuleScoutResponse(changed: WorkspaceObjectRep)
  case class GoWithRuleScout2Response(
                                       obj2: WorkspaceObjectRep,
                                       slippagesShell: SlippageListShell,
                                       object_list: List[SlipNodeRep],
                                       changedReplacementRelation: Option[String],
                                       letterCategory: SlipNodeRep
                                     )
  case class SlipnetGoWithRuleScoutResponse(string_position_category: SlipNodeRep,
                                            letter_category: SlipNodeRep
  )
  case class GoWithRuleScout3Response(object_list: List[SlipNodeRep],
                                      letterCategory: SlipNodeRep,
                                      changedReplacementRelation: Option[String]
                                     )

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
    GoWithRuleScout3Response
  }
  import models.Slipnet.{
    SlipnetProposeRule,
    SlipnetGoWithRuleScout,
    SlipnetCompleteSlippageList,
    SlipnetCompleteSlippageListResponse,
    SlipnetGoWithRuleScout3
  }
  import models.Workspace.{
    WorkspaceProposeRule,
    GoWithRuleScout2
  }

  var runTemperature: Double = 0.0
  var descriptorFacet: Option[SlipNodeRep] = None
  var descriptor: Option[SlipNodeRep] = None
  var objectCategory: Option[SlipNodeRep] = None
  var relation: Option[SlipNodeRep] = None
  var u: Double = 0.0
  var changed : WorkspaceObjectRep = null
  var obj2: WorkspaceObjectRep = null
  var object_list: List[SlipNodeRep] = null
  var changedReplacementRelation: Option[String] = None
  var letter_category: SlipNodeRep = null

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"RuleScout. Run with initial $initialString, modified: $modifiedString and target: $targetString")
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
      log.debug("GoWithRuleScoutResponse")
      changed = c
      slipnet ! SlipnetGoWithRuleScout

    case SlipnetGoWithRuleScoutResponse(string_position_category, letter_category) =>

      workspace ! GoWithRuleScout2(changed, string_position_category, letter_category)


    case GoWithRuleScout2Response(o2, slippagesShell, ol, crr, lc) =>
      changedReplacementRelation = crr
      letter_category = lc
      object_list = ol
      obj2 = o2
      slipnet ! SlipnetCompleteSlippageList(slippagesShell)

    case SlipnetCompleteSlippageListResponse(slippage_list_rep) =>
      workspace ! GoWithRuleScout3(slippage_list_rep, object_list, obj2, letter_category,changedReplacementRelation)

    case GoWithRuleScout3Response(object_list, lc, crr ) =>
      changedReplacementRelation = crr
      letter_category = lc

      if (object_list.isEmpty){
        print("no distinguishing descriptions could be found. fizzle");
        self ! Finished
      } else {
        slipnet ! SlipnetGoWithRuleScout3(object_list, changedReplacementRelation, letter_category, runTemperature)
      }

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


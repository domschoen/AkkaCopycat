package models.codelet

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import models.Random
import play.api.libs.concurrent.InjectedActorSupport


// TODO (4 days)
// RuleTranslator (almost)
// Verify CorrespondenceStrengthTester
// initialisation of Group (notably add_bond_description)

sealed trait CodeletType

object CodeletType {

  case object BondBuilder extends CodeletType
  case object DescriptionBuilder extends CodeletType
  case object GroupBuilder extends CodeletType
  case object CorrespondenceBuilder extends CodeletType
  case object RuleBuilder extends CodeletType

  case object BottomUpBondScout extends CodeletType
  case object ReplacementFinder extends CodeletType
  case object BottomUpCorrespondenceScout extends CodeletType
  case object CorrespondenceStrengthTester extends CodeletType
  case object DescriptionStrengthTester extends CodeletType
  case object BondStrengthTester extends CodeletType
  case object GroupStrengthTester extends CodeletType
  case object RuleStrengthTester extends CodeletType
  case object TopDownBondScoutDirection extends CodeletType
  case object TopDownGroupScoutDirection extends CodeletType
  case object TopDownBondScoutCategory extends CodeletType
  case object TopDownGroupScoutCategory extends CodeletType
  case object TopDownDescriptionScout extends CodeletType
  case object Breaker extends CodeletType
  case object RuleTranslator extends CodeletType
  case object RuleScout extends CodeletType
  case object ImportantObjectCorrespondenceScout extends CodeletType
  case object GroupScoutWholeString extends CodeletType
  case object BottomUpDescriptionScout extends CodeletType

}

object CodeletTypeString {
  val BondBuilder = "bond-builder"
  val DescriptionBuilder = "description-builder"
  val TopDownBondScoutDirection = "top-down-bond-scout--direction"
  val TopDownGroupScoutDirection = "top-down-group-scout--direction"
  val TopDownBondScoutCategory = "top-down-bond-scout--category"
  val TopDownGroupScoutCategory = "top-down-group-scout--category"
  val TopDownDescriptionScout = "top-down-description-scout"
  val CorrespondenceStrengthTester = "correspondence-strength-tester"
  val DescriptionStrengthTester = "description-strength-tester"
  val BondStrengthTester = "bond-strength-tester"
  val GroupStrengthTester = "group-strength-tester"
  val GroupBuilder = "group-builder"
  val CorrespondenceBuilder = "correspondence-builder"
  val RuleStrengthTester = "rule-strength-tester"
  val RuleBuilder = "bond-builder"
  val Breaker = "breaker"
  val RuleTranslator = "rule-translator"
  val RuleScout = "rule-scout"
  val ReplacementFinder = "replacement-finder"
  val ImportantObjectCorrespondenceScout = "important-object-correspondence-scout"
  val BottomUpCorrespondenceScout = "bottom-up-correspondence-scout"
  val GroupScoutWholeString = "group-scout--whole-string"
  val BottomUpBondScout = "bottom-up-bond-scout"
  val BottomUpDescriptionScout = "bottom-up-description-scout"

}

object Codelet {
  case class Run(initialString: String, modifiedString: String, targetString: String, runTemperature: Double)
  case class PrepareDescriptionResponse(descriptionID: String, urgency: Double)

  case object Finished

  def props(codeletType: CodeletType, urgency: Int, workspace: ActorRef,  slipnet: ActorRef,  temperature: ActorRef, arguments: Option[Any]): Props =
    Props(Codelet(codeletType, urgency, workspace, slipnet, temperature, arguments))


  def flipCoin(value: Double): Boolean = {
    Random.rnd(null) < value
  }

  def codeletTypeWithString(s: String): CodeletType = {
    s match {
      case CodeletTypeString.TopDownBondScoutDirection => CodeletType.TopDownBondScoutDirection
      case CodeletTypeString.TopDownGroupScoutDirection => CodeletType.TopDownGroupScoutDirection
      case CodeletTypeString.TopDownBondScoutCategory => CodeletType.TopDownBondScoutCategory
      case CodeletTypeString.TopDownGroupScoutCategory => CodeletType.TopDownGroupScoutCategory
      case CodeletTypeString.TopDownDescriptionScout => CodeletType.TopDownDescriptionScout
      case CodeletTypeString.Breaker => CodeletType.Breaker
      case CodeletTypeString.RuleTranslator => CodeletType.RuleTranslator
      case CodeletTypeString.ReplacementFinder => CodeletType.ReplacementFinder
      case CodeletTypeString.RuleScout => CodeletType.RuleScout
      case CodeletTypeString.ImportantObjectCorrespondenceScout => CodeletType.ImportantObjectCorrespondenceScout
      case CodeletTypeString.BottomUpCorrespondenceScout => CodeletType.BottomUpCorrespondenceScout
      case CodeletTypeString.GroupScoutWholeString => CodeletType.GroupScoutWholeString
      case CodeletTypeString.BottomUpBondScout => CodeletType.BottomUpBondScout
      case CodeletTypeString.BottomUpDescriptionScout => CodeletType.BottomUpDescriptionScout

    }
  }

  def stringWithCodeletType(ct: CodeletType) = {
    ct match {
      case CodeletType.BondBuilder => CodeletTypeString.BondBuilder
      case CodeletType.DescriptionBuilder => CodeletTypeString.DescriptionBuilder
      case CodeletType.BottomUpBondScout => CodeletTypeString.BottomUpBondScout
      case CodeletType.ReplacementFinder => CodeletTypeString.ReplacementFinder
      case CodeletType.BottomUpCorrespondenceScout => CodeletTypeString.BottomUpCorrespondenceScout
      case CodeletType.CorrespondenceStrengthTester => CodeletTypeString.CorrespondenceStrengthTester
      case CodeletType.DescriptionStrengthTester => CodeletTypeString.DescriptionStrengthTester
      case CodeletType.BondStrengthTester => CodeletTypeString.BondStrengthTester
      case CodeletType.GroupStrengthTester => CodeletTypeString.GroupStrengthTester
      case CodeletType.GroupBuilder => CodeletTypeString.GroupBuilder
      case CodeletType.CorrespondenceBuilder => CodeletTypeString.CorrespondenceBuilder
      case CodeletType.RuleStrengthTester => CodeletTypeString.RuleStrengthTester
      case CodeletType.RuleBuilder => CodeletTypeString.RuleBuilder
      case CodeletType.TopDownBondScoutDirection => CodeletTypeString.TopDownBondScoutDirection
      case CodeletType.TopDownGroupScoutDirection => CodeletTypeString.TopDownGroupScoutDirection
      case CodeletType.TopDownBondScoutCategory => CodeletTypeString.TopDownBondScoutCategory
      case CodeletType.TopDownGroupScoutCategory => CodeletTypeString.TopDownGroupScoutCategory
      case CodeletType.TopDownDescriptionScout => CodeletTypeString.TopDownDescriptionScout
      case CodeletType.Breaker => CodeletTypeString.Breaker
      case CodeletType.RuleTranslator => CodeletTypeString.RuleTranslator
      case CodeletType.RuleScout => CodeletTypeString.RuleScout
      case CodeletType.ImportantObjectCorrespondenceScout => CodeletTypeString.ImportantObjectCorrespondenceScout
      case CodeletType.GroupScoutWholeString => CodeletTypeString.GroupScoutWholeString
      case CodeletType.BottomUpDescriptionScout => CodeletTypeString.BottomUpDescriptionScout

    }
  }

  def apply(
             codeletType: CodeletType,
             urgency: Int,
             workspace: ActorRef,
             slipnet: ActorRef,
             temperature: ActorRef,
             arguments: Option[Any]
           ): Codelet =
    codeletType match {
      case CodeletType.BondBuilder => new BondBuilder(urgency, workspace, slipnet, temperature, arguments)
      case CodeletType.DescriptionBuilder => new DescriptionBuilder(urgency, workspace, slipnet, temperature, arguments)
      case CodeletType.BottomUpBondScout => new BottomUpBondScout(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.ReplacementFinder => new ReplacementFinder(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.BottomUpCorrespondenceScout => new BottomUpCorrespondenceScout(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.CorrespondenceStrengthTester => new CorrespondenceStrengthTester(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.DescriptionStrengthTester => new DescriptionStrengthTester(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.BondStrengthTester => new BondStrengthTester(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.GroupStrengthTester => new GroupStrengthTester(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.GroupBuilder => new GroupBuilder(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.CorrespondenceBuilder => new CorrespondenceBuilder(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.RuleStrengthTester => new RuleStrengthTester(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.RuleBuilder => new RuleBuilder(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.TopDownBondScoutDirection => new TopDownBondScoutDirection(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.TopDownGroupScoutDirection => new TopDownGroupScoutDirection(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.TopDownBondScoutCategory => new TopDownBondScoutCategory(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.TopDownGroupScoutCategory => new TopDownGroupScoutCategory(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.TopDownDescriptionScout => new TopDownDescriptionScout(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.Breaker => new Breaker(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.RuleTranslator => new RuleTranslator(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.RuleScout => new RuleScout(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.ImportantObjectCorrespondenceScout => new ImportantObjectCorrespondenceScout(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.GroupScoutWholeString => new GroupScoutWholeString(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.BottomUpDescriptionScout => new BottomUpDescriptionScout(urgency,  workspace, slipnet, temperature, arguments)


    }

}



abstract class Codelet(u: Int,
                       workspace: ActorRef,
                       slipnet: ActorRef,
                       temperature: ActorRef
                      ) extends Actor with ActorLogging with InjectedActorSupport {
  var woAppActor: Option[ActorRef] = None
  var urgency = u
  var coderack: ActorRef = null
  var t = 100.0


  override def toString: String = getClass.getName
}


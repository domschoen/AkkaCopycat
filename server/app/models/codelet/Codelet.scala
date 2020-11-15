package models.codelet

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import play.api.libs.concurrent.InjectedActorSupport

sealed trait CodeletType

object CodeletType {

  case object BondBuilder extends CodeletType
  case object BottomUpBondScout extends CodeletType
  case object ReplacementFinder extends CodeletType
  case object BottomUpCorrespondenceScout extends CodeletType
  case object CorrespondenceStrengthTester extends CodeletType
}
object Codelet {
  case class Run(initialString: String, modifiedString: String, targetString: String, runTemperature: Double)
  case object Finished

  def props(codeletType: CodeletType, urgency: Int, workspace: ActorRef,  slipnet: ActorRef,  temperature: ActorRef, arguments: Option[Any]): Props =
    Props(Codelet(codeletType, urgency, workspace, slipnet, temperature, arguments))

  val r = scala.util.Random

  def flipCoin(value: Double): Boolean = {
     r.nextDouble() < value
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
      case CodeletType.BottomUpBondScout => new BottomUpBondScout(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.ReplacementFinder => new ReplacementFinder(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.BottomUpCorrespondenceScout => new BottomUpCorrespondenceScout(urgency,  workspace, slipnet, temperature, arguments)
      case CodeletType.CorrespondenceStrengthTester => new CorrespondenceStrengthTester(urgency,  workspace, slipnet, temperature, arguments)
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



}


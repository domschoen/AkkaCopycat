package models.codelet

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import play.api.libs.concurrent.InjectedActorSupport

sealed trait CodeletType

object CodeletType {

  case object BondBuilder extends CodeletType
  case object BottomUpBondScout extends CodeletType
  case object ReplacementFinder extends CodeletType
  case object BottomUpCorrespondenceScout extends CodeletType

}
object Codelet {
  case class Run(initialString: String, modifiedString: String, targetString: String, runTemperature: Double)

  def props(codeletType: CodeletType, urgency: Int, workspace: ActorRef): Props =
    Props(Codelet(codeletType, urgency, workspace))

  val r = scala.util.Random

  def flipCoin(value: Double): Boolean = {
     r.nextDouble() < value
  }


  def apply(codeletType: CodeletType, urgency: Int, workspace: ActorRef): Codelet =
    codeletType match {
      case CodeletType.BondBuilder => new BondBuilder(urgency,workspace)
      case CodeletType.BottomUpBondScout => new BottomUpBondScout(urgency, workspace)
      case CodeletType.ReplacementFinder => new ReplacementFinder(urgency, workspace)
      case CodeletType.BottomUpCorrespondenceScout => new BottomUpCorrespondenceScout(urgency, workspace)
    }

}



abstract class Codelet(u: Int, workspace: ActorRef) extends Actor with ActorLogging with InjectedActorSupport {
  var woAppActor: Option[ActorRef] = None
  var urgency = u



}


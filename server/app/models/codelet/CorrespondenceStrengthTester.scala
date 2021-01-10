package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Correspondence.CorrespondenceRep
import models.Slipnet.{GroupFlippedVersion, GroupFlippedVersionResponse, SlipnetGoWithCorrespondenceStrengthTester}
import models.Temperature.{TemperatureChanged, TemperatureResponse}
import models.Workspace.{GoWithCorrespondenceBuilder, GoWithCorrespondenceBuilder2, GoWithCorrespondenceBuilderResponse}

// codelet.java.1438
object CorrespondenceStrengthTester {
  case class GoWithCorrespondenceStrengthTesterResponse(c: CorrespondenceRep, strength: Double)
  case object SlipnetGoWithCorrespondenceStrengthTesterResponse
}
class CorrespondenceStrengthTester(urgency: Int,              workspace: ActorRef,
                                   slipnet: ActorRef,
                                   temperature: ActorRef,
                                   arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Temperature.Register
  import CorrespondenceStrengthTester.{
    GoWithCorrespondenceStrengthTesterResponse,
    SlipnetGoWithCorrespondenceStrengthTesterResponse
  }
  import models.Coderack.PostCorrespondenceBuilder
  import models.Workspace.GoWithCorrespondenceStrengthTester

  var runTemperature: Double = 0.0
  var strength = 0.0

  def correspondanceID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"CorrespondenceStrengthTester. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t

      workspace ! GoWithCorrespondenceBuilder(runTemperature, correspondanceID)

    case GoWithCorrespondenceBuilderResponse(obj2) =>
      slipnet ! GroupFlippedVersion(obj2)

    case GroupFlippedVersionResponse(fgr) =>
      if (fgr.isEmpty) {
        println("Stuffer: empty flipped. Fizzle")
        self ! Finished
      } else {
        workspace ! GoWithCorrespondenceStrengthTester(correspondanceID, fgr.get, runTemperature)
      }

    case GoWithCorrespondenceStrengthTesterResponse(c, s) =>
      strength = s
      slipnet ! SlipnetGoWithCorrespondenceStrengthTester(c)

    case SlipnetGoWithCorrespondenceStrengthTesterResponse =>
      coderack ! PostCorrespondenceBuilder(correspondanceID,strength)



    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step

    }


}


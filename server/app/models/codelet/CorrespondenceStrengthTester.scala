package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.Temperatures
import models.Correspondence.CorrespondenceRep
import models.Slipnet.{CorrespondenceUpdateStrengthData, GroupFlippedVersion, GroupFlippedVersionResponse, SlipnetGoWithCorrespondenceStrengthTester}
import models.Workspace.{GoWithCorrespondenceBuilder, GoWithCorrespondenceBuilder2, GoWithCorrespondenceBuilder9Response, GoWithCorrespondenceBuilderResponse, GoWithCorrespondenceStrengthTester2, GoWithCorrespondenceStrengthTester3}
import models.codelet.CorrespondenceStrengthTester.{GoWithCorrespondenceStrengthTesterResponse2, GoWithCorrespondenceStrengthTesterResponse3}

// codelet.java.1438
object CorrespondenceStrengthTester {
  case object GoWithCorrespondenceStrengthTesterResponse
  case class GoWithCorrespondenceStrengthTesterResponse2(c: CorrespondenceRep, workspaceCorrespondences: List[CorrespondenceRep])
  case class GoWithCorrespondenceStrengthTesterResponse3(c: CorrespondenceRep, strenght: Double)
  case class SlipnetGoWithCorrespondenceStrengthTesterResponse(cData: CorrespondenceUpdateStrengthData)
  case object SlipnetGoWithCorrespondenceStrengthTester2Response
}
class CorrespondenceStrengthTester(urgency: Int,
                                   workspace: ActorRef,
                                   slipnet: ActorRef,
                                   arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import CorrespondenceStrengthTester.{
    GoWithCorrespondenceStrengthTesterResponse,
    SlipnetGoWithCorrespondenceStrengthTesterResponse,
    SlipnetGoWithCorrespondenceStrengthTester2Response
  }
  import models.Coderack.PostCorrespondenceBuilder
  import models.Workspace.GoWithCorrespondenceStrengthTester
  import models.Slipnet.{
    SlipnetGoWithCorrespondenceStrengthTester2
  }

  var runTemperature: Temperatures = null
  var strength = 0.0

  def correspondanceID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"CorrespondenceStrengthTester. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t

      workspace ! GoWithCorrespondenceBuilder(runTemperature, correspondanceID)

    case GoWithCorrespondenceBuilderResponse(obj2) =>
      obj2.asGroupRep match {
        case Some(gr) =>
          log.debug("CorrespondenceStrengthTester: is a Group")
          slipnet ! GroupFlippedVersion(gr)
        case None =>
          log.debug("CorrespondenceStrengthTester: is a " + obj2.getClass)
          workspace ! GoWithCorrespondenceStrengthTester2(correspondanceID,runTemperature)

      }


    case GroupFlippedVersionResponse(fgr) =>
      if (fgr.isEmpty) {
        log.debug("Stuffer: empty flipped. Fizzle")
        self ! Finished
      } else {
        log.debug("GroupFlippedVersionResponse: continue")
        workspace ! GoWithCorrespondenceBuilder2(correspondanceID, fgr.get, runTemperature)
      }

    // From GroupFlippedVersion
    case GoWithCorrespondenceBuilder9Response =>
      workspace ! GoWithCorrespondenceStrengthTester2(correspondanceID,runTemperature)

    case GoWithCorrespondenceStrengthTesterResponse2(c, workspaceCorrespondences) =>
      slipnet ! SlipnetGoWithCorrespondenceStrengthTester(c, workspaceCorrespondences)

    case SlipnetGoWithCorrespondenceStrengthTesterResponse(cData) =>
      workspace ! GoWithCorrespondenceStrengthTester3(correspondanceID,cData, runTemperature)


    case GoWithCorrespondenceStrengthTesterResponse3(c, s) =>
      strength = s
      slipnet ! SlipnetGoWithCorrespondenceStrengthTester2(c)

    case SlipnetGoWithCorrespondenceStrengthTester2Response =>
      coderack ! PostCorrespondenceBuilder(correspondanceID, strength)


    case Finished =>
      log.debug("CorrespondenceStrengthTester. Finished")
      workspace ! models.Workspace.Step(runTemperature)

    }


}


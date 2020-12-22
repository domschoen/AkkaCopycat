package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.{PostBondBuilder, PostGroupBuilder}
import models.Group.GroupRep
import models.codelet.BondStrengthTester.GoWithBondStrengthTesterResponse

object GroupStrengthTester  {
  case class GoWithGroupStrengthTesterResponse(g: GroupRep, strength: Double)
  case object SlipnetGoWithGroupStrengthTesterResponse
}


class GroupStrengthTester(urgency: Int,
                          workspace: ActorRef,
                          slipnet: ActorRef,
                          temperature: ActorRef,
                          arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import GroupStrengthTester.{
    GoWithGroupStrengthTesterResponse,
    SlipnetGoWithGroupStrengthTesterResponse
  }
  import models.Workspace.{GoWithGroupStrengthTester}
  import models.Slipnet.SlipnetGoWithGroupStrengthTester

  var runTemperature: Double = 0.0
  var strength : Double = 0.0

  def groupID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t
      workspace ! GoWithGroupStrengthTester(runTemperature, groupID)

    case GoWithGroupStrengthTesterResponse(g, s) =>
      strength = s
      slipnet ! SlipnetGoWithGroupStrengthTester(g, strength)

    case SlipnetGoWithGroupStrengthTesterResponse =>
      coderack ! PostGroupBuilder(groupID(),strength)


    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)
  }


}


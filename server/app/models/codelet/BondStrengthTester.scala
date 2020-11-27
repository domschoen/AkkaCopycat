package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import com.sun.org.apache.xerces.internal.impl.xpath.XPath.Step


object BondStrengthTester {
  case class GoWithBondStrengthTesterResponse(strength: Double)
}

class BondStrengthTester(urgency: Int,
                         workspace: ActorRef,
                         slipnet: ActorRef,
                         temperature: ActorRef,
                         arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.{ ChooseAndRun, PostBondBuilder }
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import models.Workspace.GoWithBondStrengthTester
  import BondStrengthTester.GoWithBondStrengthTesterResponse

  def bondID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      workspace ! GoWithBondStrengthTester(runTemperature, bondID)

    case GoWithBondStrengthTesterResponse(s) =>
      coderack ! PostBondBuilder(bondID,s)


    case TemperatureResponse(value) =>
    t = value

    case TemperatureChanged(value) =>
    t = value

    case Finished =>
      workspace ! models.Workspace.Step

  }


}


package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Coderack.Step
import models.Slipnet.SlipnetGoWithBondStrengthTester
import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
import models.Workspace.{GoWithBondBuilder, GoWithBondStrengthTester}


class BondBuilder(urgency: Int, workspace: ActorRef,
                  slipnet: ActorRef,
                  temperature: ActorRef,
                  arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Workspace.{
    GoWithBondStrengthTester,
    GoWithBondStrengthTesterResponse
  }
  import models.Slipnet.SlipnetGoWithBondStrengthTesterResponse

  var runTemperature = 0.0
  def bondID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, t) =>
      log.debug(s"BondBuilder. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t

      workspace ! GoWithBondStrengthTester(runTemperature, bondID)

    case GoWithBondStrengthTesterResponse(bondRep) =>
      log.debug("GoWithBondStrengthTesterResponse")
      slipnet ! SlipnetGoWithBondStrengthTester(bondRep)

    case SlipnetGoWithBondStrengthTesterResponse(bond_category_degree_of_association) =>
      log.debug("BondBuilder. SlipnetGoWithBondStrengthTesterResponse")
      workspace ! GoWithBondBuilder(runTemperature, bondID(), bond_category_degree_of_association)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


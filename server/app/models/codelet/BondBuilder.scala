package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Coderack.Step
import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
import models.Workspace.{GoWithBondBuilder}


class BondBuilder(urgency: Int, workspace: ActorRef,
                  slipnet: ActorRef,
                  temperature: ActorRef,
                  arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun

  var runTemperature = 0.0
  def bondID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, t) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t

      workspace ! GoWithBondBuilder(runTemperature, bondID)
    }
    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
import models.Workspace.GoWithBondBuilder


class BondBuilder(urgency: Int, workspace: ActorRef,
                  slipnet: ActorRef,
                  temperature: ActorRef,
                  arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, runTemperature) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)

      workspace ! GoWithBondBuilder(runTemperature, self)
    }
    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      coderack ! ChooseAndRun

  }


}


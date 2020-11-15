package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
import models.Workspace
import models.codelet.Codelet.Finished

// codelet.java.240
class BottomUpBondScout(urgency: Int,              workspace: ActorRef,
                        slipnet: ActorRef,
                        temperature: ActorRef,
                        arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.Run
  import Workspace.BondWithNeighbor
  import models.Coderack.ChooseAndRun


  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString, runTemperature) =>
      log.debug(s"BottomUpBondScout. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)

      workspace ! BondWithNeighbor(runTemperature)
    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      coderack ! ChooseAndRun

  }


}


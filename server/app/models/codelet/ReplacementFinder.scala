package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
import models.Workspace.GoWithReplacementFinder

// see codelet.java.994
class ReplacementFinder(urgency: Int,              workspace: ActorRef,
                        slipnet: ActorRef,
                        temperature: ActorRef,
                        arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun

  var runTemperature = 0.0
  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"ReplacementFinder. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      runTemperature = t

      workspace ! GoWithReplacementFinder
    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

    }


}


package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef


class RuleBuilder(urgency: Int,
                  workspace: ActorRef,
                  slipnet: ActorRef,
                  temperature: ActorRef,
                  arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)


      case TemperatureResponse(value) =>
      t = value

      case TemperatureChanged(value) =>
      t = value

      case Finished =>
        workspace ! models.Workspace.Step

    }


}


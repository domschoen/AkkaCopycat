package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive


class GroupBuilder(urgency: Int,
                   workspace: ActorRef,
                   slipnet: ActorRef,
                   temperature: ActorRef,
                   arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Workspace.GoWithGroupBuilder
  import models.Coderack.ChooseAndRun
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}

  def groupID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")

      coderack = sender()
      temperature ! Register(self)

      workspace ! GoWithGroupBuilder(runTemperature, groupID)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step

  }


}


package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Temperature.{TemperatureChanged, TemperatureResponse}
import models.Workspace.GoWithDescriptionStrengthTester


class CorrespondenceStrengthTester(urgency: Int,              workspace: ActorRef,
                                   slipnet: ActorRef,
                                   temperature: ActorRef,
                                   arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Temperature.Register

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)

      val descriptionID = arguments.get.asInstanceOf[String]
      workspace ! GoWithDescriptionStrengthTester(runTemperature, descriptionID)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
        coderack ! ChooseAndRun

    }


}


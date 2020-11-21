package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef


object DescriptionStrengthTester {
  case class GoWithDescriptionStrengthTesterResponse(strength: Double)
}

class DescriptionStrengthTester(urgency: Int,
                                workspace: ActorRef,
                                slipnet: ActorRef,
                                temperature: ActorRef,
                                arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.{ProposeCorrespondence, PostDescriptionBuilder}
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import DescriptionStrengthTester.GoWithDescriptionStrengthTesterResponse
  import models.Workspace.GoWithDescriptionStrengthTester

  def descriptionID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)

      workspace ! GoWithDescriptionStrengthTester(runTemperature, descriptionID)


    case GoWithDescriptionStrengthTesterResponse(strength) =>
      coderack ! PostDescriptionBuilder(descriptionID,strength)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      coderack ! ChooseAndRun
  }


}


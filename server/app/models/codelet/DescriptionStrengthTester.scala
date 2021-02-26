package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef


object DescriptionStrengthTester {
  case class GoWithDescriptionStrengthTesterResponse(strength: Double)
}

class DescriptionStrengthTester(urgency: Int,
                                workspace: ActorRef,
                                slipnet: ActorRef,
                                arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.{ProposeCorrespondence, PostDescriptionBuilder}
  import DescriptionStrengthTester.GoWithDescriptionStrengthTesterResponse
  import models.Workspace.GoWithDescriptionStrengthTester

  var runTemperature: models.Coderack.Temperatures  = null
  def descriptionID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t
      workspace ! GoWithDescriptionStrengthTester(runTemperature, descriptionID)


    case GoWithDescriptionStrengthTesterResponse(strength) =>
      coderack ! PostDescriptionBuilder(descriptionID,strength)


    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)
  }


}


package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive


// codelet.java.880
object GroupBuilder {

}

class GroupBuilder(urgency: Int,
                   workspace: ActorRef,
                   slipnet: ActorRef,
                   temperature: ActorRef,
                   arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Workspace.GoWithGroupBuilder
  import models.Coderack.ChooseAndRun
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}

  var runTemperature = 0.0
  def groupID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"GroupBuilder. Run with initial $initialString, modified: $modifiedString and target: $targetString")

      coderack = sender()
      temperature ! Register(self)
      runTemperature = t

      workspace ! GoWithGroupBuilder(runTemperature, groupID)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.SlipNode.SlipNodeRep
import models.Workspace.GoWithDescriptionBuilder
import models.WorkspaceObject.WorkspaceObjectRep


object DescriptionBuilder {

}

// Codelet.java.219
class DescriptionBuilder(urgency: Int,
                         workspace: ActorRef,
                         slipnet: ActorRef,
                         temperature: ActorRef,
                         arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}

  var chosen_object: WorkspaceObjectRep = null
  var runTemperature : Double = 0.0
  var chosen_property : SlipNodeRep = null

  def descriptionTypeID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      temperature ! Register(self)
      workspace ! GoWithDescriptionBuilder(descriptionTypeID, t)


    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)


  }


}


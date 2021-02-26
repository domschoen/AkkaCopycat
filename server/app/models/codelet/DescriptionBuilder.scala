package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.Temperatures
import models.SlipNode.SlipNodeRep
import models.Workspace.GoWithDescriptionBuilder
import models.WorkspaceObject.WorkspaceObjectRep


object DescriptionBuilder {

}

// Codelet.java.219
class DescriptionBuilder(urgency: Int,
                         workspace: ActorRef,
                         slipnet: ActorRef,
                         arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence

  var chosen_object: WorkspaceObjectRep = null
  var runTemperature : Temperatures = null
  var chosen_property : SlipNodeRep = null

  def descriptionTypeID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t
      workspace ! GoWithDescriptionBuilder(descriptionTypeID, t)


    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)


  }


}


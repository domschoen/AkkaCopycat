package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Coderack.ProposeDescription
import models.Workspace.{GoWithBottomUpDescriptionScout, PrepareDescription}
import models.Description.DescriptionRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.SlipnetGoWithBottomUpDescriptionScout
import models.WorkspaceObject.WorkspaceObjectRep

object BottomUpDescriptionScout{
  case class GoWithBottomUpDescriptionScoutResponse(chosen_object: WorkspaceObjectRep, d: SlipNodeRep)
  case class SlipnetGoWithBottomUpDescriptionScoutResponse(chosen_propertyRep: SlipNodeRep, description_typeRep: SlipNodeRep)
}
class BottomUpDescriptionScout(urgency: Int,
                               workspace: ActorRef,
                               slipnet: ActorRef,
                               temperature: ActorRef,
                               arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import Codelet.PrepareDescriptionResponse
  import BottomUpDescriptionScout.{
    GoWithBottomUpDescriptionScoutResponse,
    SlipnetGoWithBottomUpDescriptionScoutResponse
  }

  var chosen_object: WorkspaceObjectRep = null
  var runTemperature : Double = 0.0
  var chosen_property : SlipNodeRep = null

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      runTemperature = t
      coderack = sender()
      temperature ! Register(self)
      workspace ! GoWithBottomUpDescriptionScout(t)

    case GoWithBottomUpDescriptionScoutResponse(co, d) =>
      chosen_object = co
      slipnet ! SlipnetGoWithBottomUpDescriptionScout(d, runTemperature)


    case SlipnetGoWithBottomUpDescriptionScoutResponse(chosen_propertyRep, description_typeRep) =>
      chosen_property = chosen_propertyRep
      workspace ! PrepareDescription(chosen_object, chosen_propertyRep, description_typeRep)

    case PrepareDescriptionResponse(descriptionID, urgency) =>
      log.debug("proposing description " + chosen_property.id)
      coderack ! ProposeDescription(descriptionID, urgency)

    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)
  }


}


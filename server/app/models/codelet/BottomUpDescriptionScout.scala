package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Coderack.{ProposeDescription, Temperatures}
import models.Workspace.{GoWithBottomUpDescriptionScout, PrepareDescription}
import models.Description.DescriptionRep
import models.SlipNode.SlipNodeRep
import models.Slipnet.SlipnetGoWithBottomUpDescriptionScout
import models.WorkspaceObject.WorkspaceObjectRep

object BottomUpDescriptionScout{
  case class GoWithBottomUpDescriptionScoutResponse(chosen_object: WorkspaceObjectRep, d: SlipNodeRep)
  case class SlipnetGoWithBottomUpDescriptionScoutResponse(description_typeRep: SlipNodeRep, chosen_propertyRep: SlipNodeRep)
}
class BottomUpDescriptionScout(urgency: Int,
                               workspace: ActorRef,
                               slipnet: ActorRef,
                               arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import Codelet.PrepareDescriptionResponse
  import BottomUpDescriptionScout.{
    GoWithBottomUpDescriptionScoutResponse,
    SlipnetGoWithBottomUpDescriptionScoutResponse
  }

  var chosen_object: WorkspaceObjectRep = null
  var runTemperature : Temperatures = null
  var chosen_property : SlipNodeRep = null

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"BottomUpDescriptionScout. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      runTemperature = t
      coderack = sender()
      workspace ! GoWithBottomUpDescriptionScout(t)

    case GoWithBottomUpDescriptionScoutResponse(co, d) =>
      chosen_object = co
      slipnet ! SlipnetGoWithBottomUpDescriptionScout(d, runTemperature)


    case SlipnetGoWithBottomUpDescriptionScoutResponse(description_typeRep, chosen_propertyRep ) =>
      chosen_property = chosen_propertyRep
      workspace ! PrepareDescription(chosen_object, description_typeRep, chosen_propertyRep)

    case PrepareDescriptionResponse(descriptionID, urgency) =>
      log.debug("proposing description " + chosen_property.id)
      coderack ! ProposeDescription(descriptionID, urgency)

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)
  }


}


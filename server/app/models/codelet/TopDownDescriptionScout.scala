package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.ProposeDescription
import models.SlipNode.SlipNodeRep
import models.Slipnet.{DescriptionTypeInstanceLinksToNodeInfo, SlipnetGoWithTopDownDescriptionScout}
import models.Workspace.{GoWithTopDownDescriptionScout, GoWithTopDownDescriptionScout2, PrepareDescription}
import models.WorkspaceObject.WorkspaceObjectRep
import models.codelet.TopDownDescriptionScout.SlipnetGoWithTopDownDescriptionScoutResponse

object TopDownDescriptionScout {
  case class GoWithTopDownDescriptionScoutResponse(chosen_object: WorkspaceObjectRep)
  case class SlipnetGoWithTopDownDescriptionScoutResponse(i: DescriptionTypeInstanceLinksToNodeInfo)
  case class GoWithTopDownDescriptionScoutResponse2(chosen_property: SlipNodeRep)
  case class SlipnetGoWithTopDownDescriptionScoutResponse2(chosen_property_category: SlipNodeRep)

}
class TopDownDescriptionScout(urgency: Int,
                              workspace: ActorRef,
                              slipnet: ActorRef,
                              temperature: ActorRef,
                              arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import TopDownDescriptionScout.{
    GoWithTopDownDescriptionScoutResponse,
    GoWithTopDownDescriptionScoutResponse2,
    SlipnetGoWithTopDownDescriptionScoutResponse2
  }
  import Codelet.PrepareDescriptionResponse

  var chosen_object: WorkspaceObjectRep = null
  var runTemperature : Double = 0.0
  var chosen_property : SlipNodeRep = null

  def descriptionTypeID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      runTemperature = t
      coderack = sender()
      temperature ! Register(self)
      workspace ! GoWithTopDownDescriptionScout(descriptionTypeID, t)

    case GoWithTopDownDescriptionScoutResponse(co) =>
      chosen_object = co
      slipnet ! SlipnetGoWithTopDownDescriptionScout(chosen_object, descriptionTypeID)

    case SlipnetGoWithTopDownDescriptionScoutResponse(i) =>
      workspace ! GoWithTopDownDescriptionScout2(chosen_object,i)

    case GoWithTopDownDescriptionScoutResponse2(cp) =>
      chosen_property = cp
      slipnet ! SlipnetGoWithTopDownDescriptionScoutResponse2(chosen_property)

    case SlipnetGoWithTopDownDescriptionScoutResponse2(chosen_property_category) =>
      workspace ! PrepareDescription(chosen_object, chosen_property_category, chosen_property)


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


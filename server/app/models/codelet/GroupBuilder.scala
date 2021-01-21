package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Bond.BondRep


// codelet.java.880
object GroupBuilder {
  case class GoWithGroupBuilderResponse(bondReps: List[BondRep])
  case class PrepareBondFightingResponse(bondReps: List[BondRep], degOfAssos: Map[String, Double])

}

class GroupBuilder(urgency: Int,
                   workspace: ActorRef,
                   slipnet: ActorRef,
                   temperature: ActorRef,
                   arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Workspace.{
    GoWithGroupBuilder,
    GoWithGroupBuilder2
  }
  import models.Coderack.ChooseAndRun
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import GroupBuilder.{
    GoWithGroupBuilderResponse,
    PrepareBondFightingResponse
  }
  import models.Slipnet.PrepareBondFighting

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


    case GoWithGroupBuilderResponse(bondReps) =>
      log.debug("GoWithGroupBuilderResponse")
      slipnet ! PrepareBondFighting(groupID, bondReps)

    case PrepareBondFightingResponse(bondReps: List[BondRep], degOfAssos: Map[String, Double]) =>
      log.debug("PrepareBondFightingResponse")
      workspace ! GoWithGroupBuilder2(groupID(), bondReps, degOfAssos)



    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


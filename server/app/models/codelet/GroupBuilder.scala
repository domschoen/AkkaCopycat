package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Bond.BondRep


// codelet.java.880
object GroupBuilder {
  case class GoWithGroupBuilderResponse(incompatibleBondList: List[BondRep], group_category_id: String)
  case object GoWithGroupBuilderResponse2
  case class PrepareBondFightingResponse(bondReps: List[BondRep], degOfAssos: Map[String, Double])
  case class SlipnetGoWithGroupBuilderResponse(degree_of_association: Double)
  case class PrepareGroupFighting(incg: List[String], group_category_id_by_group_id: Map[String, String])
  case class SlipnetPrepareGroupFightingResponse(degree_of_association1: Double, degree_of_association2: Map[String, Double])
  case class GroupBuilderNoGroupFighting(incg: List[String])
}

class GroupBuilder(urgency: Int,
                   workspace: ActorRef,
                   slipnet: ActorRef,
                   temperature: ActorRef,
                   arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Workspace.{
    GoWithGroupBuilder,
    GoWithGroupBuilder2,
    GoWithGroupBuilder3,
    GoWithGroupBuilder4,
    GoWithGroupBuilder5
  }
  import models.Coderack.ChooseAndRun
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import GroupBuilder.{
    GoWithGroupBuilderResponse,
    PrepareBondFightingResponse,
    GoWithGroupBuilderResponse2,
    SlipnetGoWithGroupBuilderResponse,
    PrepareGroupFighting,
    SlipnetPrepareGroupFightingResponse,
    GroupBuilderNoGroupFighting
  }
  import models.Slipnet.{
    PrepareBondFighting,
    SlipnetGoWithGroupBuilder,
    SlipnetPrepareGroupFighting
  }

  var runTemperature = 0.0
  var incompatibleBondList: List[BondRep] = null
  var group_category_id: String = null
  var incg: List[String] = null

  def groupID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"GroupBuilder. Run with initial $initialString, modified: $modifiedString and target: $targetString")

      coderack = sender()
      temperature ! Register(self)
      runTemperature = t

      workspace ! GoWithGroupBuilder(runTemperature, groupID)

    case GoWithGroupBuilderResponse(incBondList, group_cat_id) =>
      incompatibleBondList = incBondList
      group_category_id = group_cat_id
      slipnet ! SlipnetGoWithGroupBuilder(group_category_id)

    case SlipnetGoWithGroupBuilderResponse(degree_of_association) =>
      workspace ! GoWithGroupBuilder2(groupID, degree_of_association, incompatibleBondList)


    case GoWithGroupBuilderResponse2 =>
      log.debug("GoWithGroupBuilderResponse")
      slipnet ! PrepareBondFighting(groupID, incompatibleBondList)

    case PrepareBondFightingResponse(bondReps: List[BondRep], degOfAssos: Map[String, Double]) =>
      log.debug("PrepareBondFightingResponse")
      workspace ! GoWithGroupBuilder3(groupID(), bondReps, degOfAssos)

    case GroupBuilderNoGroupFighting(incgIds) =>
      incg = incgIds
      workspace ! GoWithGroupBuilder5(groupID(), incompatibleBondList, incg)


    case PrepareGroupFighting(incgIds: List[String], group_category_id_by_group_id: Map[String, String]) =>
      log.debug("group_category_id_by_group_id " + group_category_id_by_group_id)
      incg = incgIds

      slipnet ! SlipnetPrepareGroupFighting(group_category_id, incg, group_category_id_by_group_id)

    case SlipnetPrepareGroupFightingResponse(degree_of_association1: Double, degree_of_association2: Map[String, Double]) =>
      workspace ! GoWithGroupBuilder4(groupID(), incg, degree_of_association1, degree_of_association2, incompatibleBondList)


    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


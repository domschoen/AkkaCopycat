package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Bond.BondRep
import models.Group.GroupRep
import models.Slipnet.SlipnetGoWithGroupStrengthTester
import models.codelet.GroupStrengthTester.SlipnetGoWithGroupStrengthTesterResponse


// codelet.java.880
object GroupBuilder {
  case class GoWithGroupBuilderResponse(incompatibleBondList: List[BondRep], group_category_id: String)
  case object GoWithGroupBuilderResponse2
  case class SlipnetGoWithGroupBuilderResponse(degree_of_association: Double)
  case class GroupBuilderPrepareGroupFighting(g: GroupRep, incg: List[GroupRep])
  case class SlipnetPrepareGroupFightingResponse(degree_of_association1: Double, degree_of_association2: Map[String, Double])
  case class GroupBuilderNoGroupFighting(incg: List[GroupRep])

  case class PrepareFightingGroupDataResponse(group_degree_of_association: Double)

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

    GoWithGroupBuilderResponse2,
    SlipnetGoWithGroupBuilderResponse,
    GroupBuilderPrepareGroupFighting,
    SlipnetPrepareGroupFightingResponse,
    GroupBuilderNoGroupFighting,
    PrepareFightingGroupDataResponse
  }
  import models.Slipnet.{
    PrepareBondFighting,
    SlipnetGoWithGroupBuilder,
    SlipnetPrepareGroupFighting,
    PrepareBondFightingResponse,
    PrepareGroupFighting
  }

  var runTemperature = 0.0
  var incompatibleBondList: List[BondRep] = null
  var group_category_id: String = null
  var incg: List[GroupRep] = null
  var bonds_degree_of_association: Map[String, Double] = null

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
      slipnet ! PrepareBondFighting(incompatibleBondList)

    case PrepareBondFightingResponse(degOfAssos: Map[String, Double]) =>
      log.debug("PrepareBondFightingResponse")
      bonds_degree_of_association = degOfAssos
      slipnet ! SlipnetGoWithGroupStrengthTester(group_category_id)

    case SlipnetGoWithGroupStrengthTesterResponse(group_degree_of_association) =>
      workspace ! GoWithGroupBuilder3(groupID(), incompatibleBondList, group_degree_of_association, bonds_degree_of_association)

    case GroupBuilderNoGroupFighting(incgReps) =>
      incg = incgReps
      workspace ! GoWithGroupBuilder5(groupID(), incompatibleBondList, incg)


    case GroupBuilderPrepareGroupFighting(g, incgIds: List[GroupRep]) =>
      incg = incgIds
      slipnet ! PrepareGroupFighting(g, incg)

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


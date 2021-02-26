package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Coderack.{PostBondBuilder, PostGroupBuilder, Temperatures}
import models.Group.GroupRep

object GroupStrengthTester  {
  case class GoWithGroupStrengthTesterResponse(group_category_id : String)
  case class GoWithGroupStrengthTesterResponse2(g: GroupRep, strength: Double)
  case class SlipnetGoWithGroupStrengthTesterResponse(degree_of_association : Double)
  case object SlipnetGoWithGroupStrengthTesterResponse2

}


class GroupStrengthTester(urgency: Int,
                          workspace: ActorRef,
                          slipnet: ActorRef,
                          arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import GroupStrengthTester.{
    GoWithGroupStrengthTesterResponse,
    GoWithGroupStrengthTesterResponse2,
    SlipnetGoWithGroupStrengthTesterResponse,
    SlipnetGoWithGroupStrengthTesterResponse2
  }
  import models.Workspace.{
    GoWithGroupStrengthTester,
    GoWithGroupStrengthTester2
  }
  import models.Slipnet.{
    SlipnetGoWithGroupStrengthTester,
    SlipnetGoWithGroupStrengthTester2
  }

  var runTemperature: Temperatures = null
  var strength : Double = 0.0

  def groupID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t
      workspace ! GoWithGroupStrengthTester(runTemperature, groupID)


    case GoWithGroupStrengthTesterResponse(group_category_id) =>
      slipnet ! SlipnetGoWithGroupStrengthTester(group_category_id)

    case SlipnetGoWithGroupStrengthTesterResponse(degree_of_association) =>
      workspace ! GoWithGroupStrengthTester2(runTemperature, groupID, degree_of_association)

    case GoWithGroupStrengthTesterResponse2(g, s) =>
      strength = s
      slipnet ! SlipnetGoWithGroupStrengthTester2(g, strength)

    case SlipnetGoWithGroupStrengthTesterResponse2 =>
      coderack ! PostGroupBuilder(groupID(),strength)

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)
  }


}


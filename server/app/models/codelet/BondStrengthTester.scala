package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import com.sun.org.apache.xerces.internal.impl.xpath.XPath.Step
import models.Bond.BondRep


object BondStrengthTester {
  case class GoWithBondStrengthTesterResponse2(strength: Double)
}

class BondStrengthTester(urgency: Int,
                         workspace: ActorRef,
                         slipnet: ActorRef,
                         arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.{ ChooseAndRun, PostBondBuilder }
  import models.Coderack.ProposeCorrespondence
  import models.Workspace.{
    GoWithBondStrengthTester,
    GoWithBondStrengthTester2,
    GoWithBondStrengthTesterResponse
  }
  import models.Slipnet.{
    SlipnetGoWithBondStrengthTesterResponse,
    SlipnetGoWithBondStrengthTester
  }
  import BondStrengthTester.{
    GoWithBondStrengthTesterResponse2

  }

  var runTemperature: models.Coderack.Temperatures  = null
  def bondID() = arguments.get.asInstanceOf[String]

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t
      workspace ! GoWithBondStrengthTester(runTemperature, bondID)

    case GoWithBondStrengthTesterResponse(bondRep) =>
      slipnet ! SlipnetGoWithBondStrengthTester(bondRep)

    case SlipnetGoWithBondStrengthTesterResponse(bond_category_degree_of_association) =>
      workspace ! GoWithBondStrengthTester2(runTemperature, bondID, bond_category_degree_of_association)

    case GoWithBondStrengthTesterResponse2(s) =>
      coderack ! PostBondBuilder(bondID,s)


    case Finished =>
      log.debug("BondStrengthTester. Finished")
      workspace ! models.Workspace.Step(runTemperature)

  }


}


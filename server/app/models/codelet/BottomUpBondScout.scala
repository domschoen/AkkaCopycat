package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive
import models.Workspace

// codelet.java.240
class BottomUpBondScout(urgency: Int, workspace: ActorRef) extends Codelet(urgency, workspace)  {
  import Codelet.Run
  import Workspace.BondWithNeighbor

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      workspace ! BondWithNeighbor(runTemperature)
    }
  }


}


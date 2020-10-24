package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef


class TopDownGroupScoutDirection(urgency: Int, workspace: ActorRef) extends Codelet(urgency, workspace)  {
  import Codelet.Run

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
    }
  }


}


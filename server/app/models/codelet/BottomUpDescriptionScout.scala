package models.codelet

import akka.actor.ActorRef
import akka.event.LoggingReceive


class BottomUpDescriptionScout(urgency: Int, workspace: ActorRef) extends Codelet(urgency, workspace)  {
  import Codelet.Run

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
    }
  }


}


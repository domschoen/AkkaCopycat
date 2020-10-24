package models.codelet

import akka.actor.{ActorRef, Props}
import akka.event.LoggingReceive
import models.Workspace


object Breaker {
  def props(urgency: Int, workspace: ActorRef): Props = Props(new Breaker(urgency, workspace))


}
class Breaker(urgency: Int, workspace: ActorRef) extends Codelet(urgency, workspace)  {
  import Codelet.Run
  import Workspace.ChooseRandomStructure

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,runTemperature) => {
      log.debug(s"Run with initial $initialString, modified: $modifiedString and target: $targetString")
      val probability = (100.0 - runTemperature) / 100.0
      log.debug("deciding whether or not to fizzle.")
      log.debug(s"fizzle probability = $probability")

      if(Codelet.flipCoin(probability)) {
        log.debug("decided to fizzle!")
      } else {
        log.debug("did not fizzle")
        // choose a structure at random
        workspace ! ChooseRandomStructure
      }
    }
  }


}


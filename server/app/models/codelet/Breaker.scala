package models.codelet

import akka.actor.{ActorRef, Props}
import akka.event.LoggingReceive
import models.Workspace



class Breaker(urgency: Int,
              workspace: ActorRef,
              slipnet: ActorRef,
              arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import Workspace.GoWithBreaker
  var runTemperature: models.Coderack.Temperatures  = null
  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      runTemperature = t
      val probability = (100.0 - runTemperature.formulaT) / 100.0
      log.debug("deciding whether or not to fizzle.")
      log.debug(s"fizzle probability = $probability")
      if(Codelet.flipCoin(probability)) {
        log.debug("decided to fizzle!")
        self ! Finished
      } else {
        log.debug("did not fizzle")
        coderack = sender()

        // choose a structure at random
        // Codelet.java.68
        workspace ! GoWithBreaker(runTemperature)
      }

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


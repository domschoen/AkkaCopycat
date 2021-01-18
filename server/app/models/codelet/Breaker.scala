package models.codelet

import akka.actor.{ActorRef, Props}
import akka.event.LoggingReceive
import models.Workspace



class Breaker(urgency: Int,
              workspace: ActorRef,
              slipnet: ActorRef,
              temperature: ActorRef,
              arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet, temperature)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun
  import models.Coderack.ProposeCorrespondence
  import models.Temperature.{Register, TemperatureChanged, TemperatureResponse}
  import Workspace.GoWithBreaker
  var runTemperature = 0.0
  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"${getClass.getName}. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      runTemperature = t
      val probability = (100.0 - runTemperature) / 100.0
      log.debug("deciding whether or not to fizzle.")
      log.debug(s"fizzle probability = $probability")
      if(Codelet.flipCoin(probability)) {
        log.debug("decided to fizzle!")
      } else {
        log.debug("did not fizzle")
        coderack = sender()
        temperature ! Register(self)

        // choose a structure at random
        // Codelet.java.68
        workspace ! GoWithBreaker(runTemperature)
      }
    case TemperatureResponse(value) =>
      t = value

    case TemperatureChanged(value) =>
      t = value

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

  }


}


package models.codelet

import akka.event.LoggingReceive
import akka.actor.ActorRef
import models.Workspace.GoWithReplacementFinder

// see codelet.java.994
class ReplacementFinder(urgency: Int,
                        workspace: ActorRef,
                        slipnet: ActorRef,
                        arguments: Option[Any]) extends Codelet(urgency, workspace, slipnet)  {
  import Codelet.{ Run, Finished }
  import models.Coderack.ChooseAndRun

  var runTemperature: models.Coderack.Temperatures  = null
  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString,t) =>
      log.debug(s"ReplacementFinder. Run with initial $initialString, modified: $modifiedString and target: $targetString")
      coderack = sender()
      runTemperature = t

      workspace ! GoWithReplacementFinder

    case Finished =>
      workspace ! models.Workspace.Step(runTemperature)

    }


}


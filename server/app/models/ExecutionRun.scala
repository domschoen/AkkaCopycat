package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props




object ExecutionRun {
  def props(): Props = Props(new ExecutionRun())

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case object Found
  case object Step
  case object UpdateEverything

}


class ExecutionRun extends Actor with ActorLogging  { //with InjectedActorSupport
  import ExecutionRun._
  import Slipnet.InitializeSlipnet
  import Workspace.Initialize
  var workspace: ActorRef = null
  var slipnet: ActorRef = null
  var coderack: ActorRef = null
  var temperature: ActorRef = null

  def receive = LoggingReceive {
    // to the browser
    case Run(initialString, modifiedString, targetString) => {
      log.debug(s"ExecutionRun: Run with initial $initialString, modified: $modifiedString and target: $targetString")
      slipnet = context.actorOf(Slipnet.props(),"Slipnet")

      temperature = context.actorOf(Temperature.props(),"Temperature")
      workspace = context.actorOf(Workspace.props(slipnet, temperature),"Workspace")
      coderack = context.actorOf(Coderack.props(workspace, slipnet, temperature, self),"Coderack")
      slipnet ! InitializeSlipnet(coderack, workspace)
      workspace ! Initialize(coderack)
      coderack ! Coderack.Run(initialString, modifiedString, targetString)
    }

    case Found => {
      log.debug("Solution found")
    }

    case UpdateEverything => {
      // TODO
    }
  }


}


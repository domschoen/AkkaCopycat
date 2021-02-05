package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props
import models.Slipnet.isNumber
import models.Workspace.InitializeWorkspace




object ExecutionRun {
  def props(): Props = Props(new ExecutionRun())

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case object Found
  case object Step
  case object UpdateEverything
  case object InitializeSlipnetResponse
}


class ExecutionRun extends Actor with ActorLogging  { //with InjectedActorSupport
  import ExecutionRun._
  import Slipnet.InitializeSlipnet
  import Workspace.Initialize
  var workspace: ActorRef = null
  var slipnet: ActorRef = null
  var coderack: ActorRef = null
  var temperature: ActorRef = null
  var initialString: String = null
  var modifiedString: String = null
  var targetString: String = null

  def receive = LoggingReceive {
    // to the browser
    case Run(is, ms, ts) => {
      initialString = is
      modifiedString = ms
      targetString = ts

      log.debug(s"ExecutionRun: Run with initial $initialString, modified: $modifiedString and target: $targetString")
      workspace = context.actorOf(Workspace.props(temperature),"Workspace")

      slipnet = context.actorOf(Slipnet.props(workspace),"Slipnet")
      workspace ! InitializeWorkspace(slipnet)
      temperature = context.actorOf(Temperature.props(),"Temperature")
      coderack = context.actorOf(Coderack.props(workspace, slipnet, temperature, self),"Coderack")
      slipnet ! InitializeSlipnet(coderack, workspace)
    }

    case InitializeSlipnetResponse =>
      coderack ! Coderack.Run(initialString, modifiedString, targetString)

    case Found => {
      log.debug("Solution found")
    }

    case UpdateEverything => {
      // TODO
    }
  }


}


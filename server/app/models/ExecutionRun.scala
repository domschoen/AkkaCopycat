package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props
import models.Workspace.InitializeWorkspace




object ExecutionRun {
  def props(): Props = Props(new ExecutionRun())

  case class Run(initialString: String, modifiedString: String, targetString: String)
  case class Found(answer:String, codelets_run: Int)
  case object Step
  case object UpdateEverything
  case object InitializeSlipnetResponse
}


class ExecutionRun extends Actor with ActorLogging  { //with InjectedActorSupport
  import ExecutionRun._
  import Slipnet.InitializeSlipnet
  var workspace: ActorRef = null
  var slipnet: ActorRef = null
  var coderack: ActorRef = null
  var initialString: String = null
  var modifiedString: String = null
  var targetString: String = null

  var runRequester: ActorRef = null

  def receive = LoggingReceive {
    // to the browser
    case Run(is, ms, ts) => {
      runRequester = sender()
      initialString = is
      modifiedString = ms
      targetString = ts

      log.debug(s"ExecutionRun: Run with initial $initialString, modified: $modifiedString and target: $targetString")
      workspace = context.actorOf(Workspace.props(),"Workspace")

      slipnet = context.actorOf(Slipnet.props(workspace),"Slipnet")
      workspace ! InitializeWorkspace(slipnet)
      coderack = context.actorOf(Coderack.props(workspace, slipnet, self),"Coderack")
      slipnet ! InitializeSlipnet(coderack, workspace)
    }

    case InitializeSlipnetResponse =>
      coderack ! Coderack.Run(initialString, modifiedString, targetString)

    case Found(answer, codelets_run) => {
      log.debug("Solution found " + answer + " after running " +codelets_run + " codelets")
      runRequester ! WebSocketActor.Found(answer, codelets_run)
    }

    case UpdateEverything => {
      // TODO
    }
  }


}


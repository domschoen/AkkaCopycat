package models

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import com.typesafe.config.ConfigFactory
import d2spa.shared.WebSocketMessages._



object WebSocketActor {
  case class Found(answer: String, codelets_run: Int)
  def props(out: ActorRef): Props = Props(new WebSocketActor(out))

}

class WebSocketActor (out: ActorRef) extends Actor with ActorLogging {
  val config = ConfigFactory.load()
  var run: Option[ActorRef] = None

  import models.WebSocketActor.Found

  def receive = {
    /*case InstancesResponse(app, instances: Option[List[Instance]]) =>
      log.debug("Receive InstancesResponse ---> sending InstancesResponse")
      out ! InstancesResponseMsg(app, instances)*/

    case Found(answer, codelets_run) =>
      log.debug(s"WebSocketActor received answer $answer after running $codelets_run codelets")


    case msg: WebSocketMsgIn => msg match {

      case Run(initialString,modifiedString,targetString) =>
        log.info("Websocket received Run")
        if (run.isDefined) {
          log.debug("Run exists => Poinson pill it !")
          run.get ! PoisonPill
        }
        run = Some(context.actorOf(ExecutionRun.props(), "ExecutionRun"))
        run.get ! ExecutionRun.Run(initialString,modifiedString,targetString)
    }
  }
}

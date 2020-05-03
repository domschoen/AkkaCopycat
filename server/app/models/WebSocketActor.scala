package models

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import com.typesafe.config.ConfigFactory
import d2spa.shared._
import d2spa.shared.WebSocketMessages._
import javax.inject.Inject
import play.api.{Configuration, Logger}
import play.api.libs.concurrent.InjectedActorSupport
import play.api.libs.ws.WSClient

import scala.concurrent.duration._
import scala.concurrent._
import ExecutionContext.Implicits.global

class WebSocketActor (out: ActorRef) extends Actor with ActorLogging {
  val config = ConfigFactory.load()
  var workspace: Option[ActorRef] = None


  def receive = {
    /*case InstancesResponse(app, instances: Option[List[Instance]]) =>
      println("Receive InstancesResponse ---> sending InstancesResponse")
      out ! InstancesResponseMsg(app, instances)*/

    case msg: WebSocketMsgIn => msg match {
      case Run(initialString,modifiedString,targetString) =>
        println("Websocket received Run")
        if (workspace.isDefined) {
          println("Workspace exists => Poinson pill it !")
          workspace.get ! PoisonPill
        }
        workspace = Some(context.actorOf(Workspace.props()))
        workspace.get ! Workspace.Run(initialString,modifiedString,targetString)
    }
  }
}

object WebSocketActor {
  def props(out: ActorRef): Props = Props(new WebSocketActor(out))
}

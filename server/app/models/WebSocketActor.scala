package models

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import d2spa.shared._
import d2spa.shared.WebSocketMessages._
import javax.inject.Inject
import models.NodeActor.SetItUp
import models.WOAppActor.InstancesResponse
import play.api.{Configuration, Logger}
import play.api.libs.concurrent.InjectedActorSupport
import play.api.libs.ws.WSClient

import scala.concurrent.duration._
import scala.concurrent._
import ExecutionContext.Implicits.global

class WebSocketActor (out: ActorRef, nodeActor: ActorRef) extends Actor with ActorLogging {
  val config = ConfigFactory.load()
  val showDebugButton = if (config.getIsNull("d2spa.showDebugButton")) true else config.getBoolean("d2spa.showDebugButton")


  /*val eomodelActor = context.actorOf(EOModelActor.props(), "eomodelFetcher")
val menusActor = context.actorOf(MenusActor.props(eomodelActor), "menusFetcher")
val rulesActor = context.actorOf(RulesActor.props(eomodelActor), "rulesFetcher")
val eoRepoActor = context.actorOf(EORepoActor.props(eomodelActor), "eoRepo")*/

  override def preStart: Unit = {
    nodeActor ! SetItUp
  }


  def receive = {
    case InstancesResponse(app, instances: Option[List[Instance]]) =>
      println("Receive InstancesResponse ---> sending InstancesResponse")
      out ! InstancesResponseMsg(app, instances)

    case msg: WebSocketMsgIn => msg match {
      case GetInstances(app) =>
        context.actorSelection("akka://application/user/node-actor/woApp") ! WOAppActor.GetInstances(app, self)
    }
  }
}

object WebSocketActor {
  def props(out: ActorRef, nodeActor: ActorRef): Props = Props(new WebSocketActor(out, nodeActor))
}

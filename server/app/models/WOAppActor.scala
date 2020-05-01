package models

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.event.LoggingReceive
import akka.actor.ActorRef
import akka.actor.Props
import com.typesafe.config.ConfigFactory
import d2spa.client.services.AdminActionUtils

import scala.xml.Utility
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import d2spa.shared.{D2WContext, Definition, EOEntity, EOJoin, EOModel, EORelationship, Instance, MonitorAction}

import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.Play.current
import javax.inject._
import models.WOAppActor.{DefineMonitorActions, GetInstances, InstancesResponse, RunActions}
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.ws.ahc._
import play.shaded.ahc.org.asynchttpclient.AsyncHttpClient

case class FetchedEOEntity(
                            name: String,
                            primaryKeyAttributeNames: Seq[String],
                            attributes: Seq[FetchedEOAttribute] = Seq(),
                            relationships: Seq[FetchedEORelationship] = Seq()
                          )

case class FetchedEORelationship(joins: Seq[FetchedEOJoin] = Seq(),
                                 name: String,
                                 definition: Option[String],
                                 isToMany: Boolean,
                                 destinationEntityName: String)

case class FetchedEOAttribute(`type`: String, name: String)

case class FetchedEOJoin(sourceAttribute: FetchedEOAttribute, destinationAttribute: FetchedEOAttribute)

case class RunAction(instance: Int, action: String)

object WOAppActor {
  def props(ws: WSClient): Props = Props(new WOAppActor(ws))

  case class InstancesResponse(app: String, instances: Option[List[Instance]])
  case class GetInstances(definition: Definition, requester: ActorRef)
  case class DefineMonitorActions(definition: Definition, instances:  Option[List[Instance]], requester: ActorRef)
  case class RunActions(definition: Definition, actions: List[RunAction], requester: ActorRef)
}


class WOAppActor (ws: WSClient) extends Actor with ActorLogging  {
  val timeout = 10.seconds
  val configuration = ConfigFactory.load()
  val d2spaServerBaseUrl = configuration.getString("d2spa.woappURL")


  override def preStart {
    println("EOModelActor Actors: preStart: self : " + self)
  }


  implicit lazy val fetchedEOEntityReads: Reads[FetchedEOEntity] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "primaryKeyAttributeNames").read[Seq[String]] and
      (
        (JsPath \ "attributes").lazyRead(Reads.seq(fetchedEOAttributeReads)) or
          Reads.pure(Seq.empty[FetchedEOAttribute])
        ) and
      (
        (JsPath \ "relationships").lazyRead(Reads.seq(fetchedEORelationshipReads)) or
          Reads.pure(Seq.empty[FetchedEORelationship])
        )
    ) (FetchedEOEntity.apply _)

  implicit lazy val fetchedEORelationshipReads: Reads[FetchedEORelationship] = (
    ((JsPath \ "joins").lazyRead(Reads.seq(fetchedEOJoinReads)) or
      Reads.pure(Seq.empty[FetchedEOJoin])
      ) and
      (JsPath \ "name").read[String] and
      (JsPath \ "definition").readNullable[String]  and
      (JsPath \ "isToMany").read[Boolean] and
      (JsPath \ "destinationEntityName").read[String]
    ) (FetchedEORelationship.apply _)

  implicit lazy val fetchedEOAttributeReads: Reads[FetchedEOAttribute] = (
    (JsPath \ "type").read[String] and
      (JsPath \ "name").read[String]
    ) (FetchedEOAttribute.apply _)

  implicit lazy val fetchedEOJoinReads: Reads[FetchedEOJoin] = (
    (JsPath \ "sourceAttribute").read[FetchedEOAttribute] and
      (JsPath \ "destinationAttribute").read[FetchedEOAttribute]
    ) (FetchedEOJoin.apply _)


  def executeEOModelWS(): Future[EOModel] = {
    val url = d2spaServerBaseUrl + "/EOModel.json";
    Logger.debug("WS " + url)
    val request: WSRequest = ws.url(url).withRequestTimeout(timeout)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>

      val resultBody = response.json
      //Logger.debug("Eomodels " + resultBody)
      var entities = List[EOEntity]()

      val modelArray = resultBody.asInstanceOf[JsArray].value
      for (model <- modelArray) {
        val eomodelJsObj = model.asInstanceOf[JsObject]
        val array = (eomodelJsObj \ "entities").get.asInstanceOf[JsArray].value
        //Logger.debug("Entities " + array)

        for (menuRaw <- array) {
          //Logger.debug(menuRaw)
          val obj = menuRaw.validate[FetchedEOEntity]
          obj match {
            case s: JsSuccess[FetchedEOEntity] => {
              val fetchedEOEntity = s.get
              val fetchedRelationships = fetchedEOEntity.relationships
              val relationships = fetchedRelationships.map(
                r => {
                  val joins = r.joins map (join => {
                    EOJoin(join.sourceAttribute.name, join.destinationAttribute.name)
                  })

                  EORelationship(joins.toList, r.name, r.definition, r.isToMany, r.destinationEntityName)
                }).toList
              val attributes: List[String] = fetchedEOEntity.attributes.map {
                a => a.name
              }.toList
              entities = EOEntity(fetchedEOEntity.name, fetchedEOEntity.primaryKeyAttributeNames.toList, attributes, relationships) :: entities
            }
            case e: JsError => Logger.error("Errors: " + JsError.toJson(e).toString())
          }
        }
      }
      //Logger.debug("Entities " + entities)
      EOModel(entities)
    }
  }

  var fetchedEOModel: Option[EOModel] = None



  def updateInstanceWithMonitorActions(definition: Definition, instancesOpt: Option[List[Instance]]):Option[List[Instance]] = {
    instancesOpt match {
      case Some(instances) =>
        val sortedInstances = instances.sortBy(_.id)
        val instancesByHost = sortedInstances.groupBy(i => i.host)
        val allocations = definition.allocations
        val updatedInstances = allocations.keys.map(server => {
          val count = allocations(server)
          val (instancesToStart, instancesToStop) = instancesByHost(server).splitAt(count)
          val updatedInstancesToStart = instancesToStart.map(i => {
            if (i.scheduled) {
              // check if refuse new session and active session transations <= active Session
              if (i.refusingNewSessions) {

                if (i.transactions <= i.activeSessions || i.transactions <= 6 ||
                  (i.activeSessions <= 4 && i.transactions % i.activeSessions == 0)
                ) {
                  i.copy(monitorActions = MonitorAction.stop :: i.monitorActions)
                } else i
              }  else i

            } else {
              i.copy(monitorActions = List(MonitorAction.turnScheduledOn))
            }
          })
          val updatedInstancesToStop = instancesToStop.map(i => {
            if (i.scheduled) {
              i.copy(monitorActions = List(MonitorAction.turnScheduledOff, MonitorAction.turnAutoRecoverOff,MonitorAction.stop))

            } else {
              if (i.autoRecover) {
                i.copy(monitorActions = List(MonitorAction.turnAutoRecoverOff,MonitorAction.stop))
              } else if (i.state.equals("ALIVE")) {
                i.copy(monitorActions = List(MonitorAction.stop))
              } else i
            }
          })
          updatedInstancesToStart ::: updatedInstancesToStop
        })


        val uis = updatedInstances.toList.flatten
        Some(uis)
      case None => None
    }
  }


  def actionsWithInstances(updatedInstances: Option[List[Instance]]): Option[List[RunAction]] = {
    updatedInstances match {
      case Some(instances) =>
        val instancesWithActions = instances.filter(!_.monitorActions.isEmpty)
        instancesWithActions match {
          case Nil => None
          case is: List[Instance] => Some(is.map(i => RunAction(i.id, i.monitorActions.head)))
        }
      case None => None
    }
  }


  def receive = LoggingReceive {

    // to rename ensure
    case GetInstances(definition, requester) => {
      println("GetInstances")
      AdminActionUtils.getInstances(definition, ws).map(instances =>
        self ! DefineMonitorActions(definition, instances, requester)
      )
    }


    case DefineMonitorActions(definition, instances, requester) => {
      println("DefineMonitorActions")
      val app = definition.app
      val updatedInstances = updateInstanceWithMonitorActions(definition, instances)
      requester ! InstancesResponse(app, updatedInstances)
      val actionsOpt = actionsWithInstances(updatedInstances)
      actionsOpt match {
        case Some(actions) =>  self ! RunActions(definition, actions, requester)
        case None =>  context.system.scheduler.scheduleOnce(10 second, self, GetInstances(definition, requester))
      }
    }

    case RunActions(definition, actions, requester) => {
      println("RunActions")
      val app = definition.app
      actions match {
        case Nil => context.system.scheduler.scheduleOnce(1 second, self, GetInstances(definition, requester))
        case x:: xs =>
          log.debug(s"Run action $x")
          AdminActionUtils.runAction(ws,app,x.instance, x.action).map(result =>
            context.system.scheduler.scheduleOnce(1 second, self, RunActions(definition, xs, requester))
          )
      }
    }
  }

}


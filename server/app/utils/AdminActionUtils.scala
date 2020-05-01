package d2spa.client.services

import d2spa.shared.{Definition, EO, EOKeyValueQualifier, EOValue, Instance, IntValue, MonitorAction, NSSelector, StringValue}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import upickle.default._
import upickle.default.{macroRW, ReadWriter => RW}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Logger
import play.api.libs.json.JsArray
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}

case class ReadInstance (
                     name: String,
                     id: String,
                     host: String,
                     port: String,
                     state: String,
                     deaths: String,
                     transactions: String,
                     activeSessions: String,
                     refusingNewSessions: Boolean,
                     scheduled: Boolean,
                     autoRecover: String
                   )

object ReadInstance {
  implicit def rw: RW[ReadInstance] = macroRW
}


object AdminActionUtils {

  /*case class Instance (
                          name: String,
                          id: Int,
                          host: String,
                          port: Int,
                          state: String,
                          deaths: Int,
                          refusingNewSessions: Boolean,
                          scheduled: Boolean,
                          autoRecover: Boolean
                        )
*/

  def convertReadInstanceToInstance(readInstance: ReadInstance): Instance = {
    val id = readInstance.id.toInt
    val autoRecoverRaw = readInstance.autoRecover.toBoolean
    val scheduled = readInstance.scheduled
    val autoRecover = if (scheduled) true else autoRecoverRaw
    Instance(
      readInstance.name,
      readInstance.id.toInt,
      readInstance.host,
      readInstance.port.toInt,
      readInstance.state,
      readInstance.deaths.toInt,
      readInstance.transactions.toInt,
      readInstance.activeSessions.toInt,
      readInstance.refusingNewSessions,
      scheduled,
      autoRecover,
      List()
    )
  }

  def getInstances(definition: Definition, ws: WSClient): Future[Option[List[Instance]]] = {
    getReadInstances(definition,ws).map( x => {
      x match {
        case Some(readInstances) => Some(readInstances.map(convertReadInstanceToInstance _).toList)
        case None => None
      }
    })
  }

  // Example:
  // http://ist.hq.k.grp:2800/cgi-bin/WebObjects/JavaMonitor.woa/admin/info?type=app&name=ist

  // Output rows like this:
  // [{"name": "ist", "id": "1", "host": "chx-istprod-01.hq.k.grp", "port": "2001", "state": "ALIVE",
  // "deaths": "8", "refusingNewSessions": false, "scheduled": true, "schedulingHourlyStartTime": 0,
  // "schedulingDailyStartTime": null, "schedulingWeeklyStartTime": 3, "schedulingType": "HOURLY",
  // "schedulingStartDay": null, "schedulingInterval": 12, "transactions": "1", "activeSessions": "0",
  // "averageIdlePeriod": "24084.004", "avgTransactionTime": "0.0","autoRecover": "true"},
  def getReadInstances(definition: Definition, ws: WSClient): Future[Option[Seq[ReadInstance]]] = {
    val app = definition.app
    Logger.info("fetch instances info for app " + app)
    //implicit def rw: OptionPickler.ReadWriter[Customer] = OptionPickler.macroRW

    val url = s"http://ist.hq.k.grp:2800/cgi-bin/WebObjects/JavaMonitor.woa/admin/info?type=app&name=$app"
    val request: WSRequest = ws.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>
      val instances = read[Seq[ReadInstance]](response.body)
      Logger.info(s"fetch instances info for app $app size ${instances.size}")

      Some(instances)
    }

  }


  // Examples:
  // http://ist.hq.k.grp:2800/cgi-bin/WebObjects/JavaMonitor.woa/admin/turnRefuseNewSessionsOn?type=ins&name=ist-162
  // http://ist.hq.k.grp:2800/cgi-bin/WebObjects/JavaMonitor.woa/admin/turnScheduledOn?type=ins&name=ist-21

  def runAction(ws: WSClient, app: String, instance: Int, action: String): Future[Boolean] = {

    Logger.info(s"runAction app: $app instance: $instance action: $action")
    //implicit def rw: OptionPickler.ReadWriter[Customer] = OptionPickler.macroRW

    val url = s"http://ist.hq.k.grp:2800/cgi-bin/WebObjects/JavaMonitor.woa/admin/$action?type=ins&name=$app-$instance"
    val request: WSRequest = ws.url(url).withRequestTimeout(10000.millis)
    val futureResponse: Future[WSResponse] = request.get()
    futureResponse.map { response =>
      Logger.info(s"runAction app: $app instance: $instance action: $action get: ${response.body}")
      response.body == "OK"
    }

  }


}



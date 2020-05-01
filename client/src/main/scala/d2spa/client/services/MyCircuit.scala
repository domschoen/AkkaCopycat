package d2spa.client.services

import autowire._
import diode._
import diode.data._
import diode.util._
import diode.react.ReactConnector
import diode.ActionResult.ModelUpdate
import diode.ActionResult.ModelUpdateEffect
import d2spa.shared._
import boopickle.Default._
import d2spa.client.EOCacheUtils.updatedMemCacheByRemovingEO

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import japgolly.scalajs.react.extra.router.RouterCtl
import d2spa.client._
import d2spa.client.logger._

import scala.collection.immutable.Set

/*import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import japgolly.scalajs.react.extra.router._*/

import d2spa.client.AppModel

import d2spa.shared.{Menus, EntityMetaData, PropertyMetaInfo, EO, EOValue}

object MyCircuit extends Circuit[AppModel] with ReactConnector[AppModel] {
  // define initial value for the application model
  override protected def initialModel = AppModel.bootingModel

  override val actionHandler = composeHandlers(
    new InstancesHandler(zoomTo(_.content.temperature))
  )

}

class InstancesHandler[M](modelRW: ModelRW[M, Int]) extends ActionHandler(modelRW) {

  override def handle = {

    case ChangeTemperature =>
      val newValue = value + 1
      println("new temperature " + newValue)
      updated(newValue)


   }
}


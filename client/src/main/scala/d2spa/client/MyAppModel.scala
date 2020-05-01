package d2spa.client

import diode._
import diode.data._
import diode.util._
import d2spa.shared.{EntityMetaData, _}
import boopickle.DefaultBasic._
import d2spa.client.logger.log
import jdk.nashorn.internal.ir.PropertyKey
/**
  * Created by dschoen on 01.05.17.
  */



case class AppModel (content: MegaContent)



case class MegaContent(
    temperature: Int
)



object AppModel {
  val bootingModel = AppModel(
    MegaContent(
      temperature = 0
    )
  )

}


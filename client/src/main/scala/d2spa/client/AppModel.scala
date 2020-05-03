package d2spa.client

import diode.Action


case class AppModel (content: MegaContent)



case class MegaContent(
                        temperature: Int
                      )

case class Run(initialString: String, modifiedString: String, targetString: String) extends Action
case class SetTemperature(temparature: Int) extends Action

object AppModel {
  val bootingModel = AppModel(
    MegaContent(
      temperature = 0
    )
  )

}

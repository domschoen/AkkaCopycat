package d2spa.client

import diode.Action


case class AppModel (content: MegaContent)



case class MegaContent(
                        temperature: Int,
                        answer: Option[String]
                      )

case class Run(initialString: String, modifiedString: String, targetString: String) extends Action
case class SetTemperature(temparature: Int) extends Action
case class SetAnswer(answer: String) extends Action
case object InitClient extends Action

object AppModel {
  val bootingModel = AppModel(
    MegaContent(
      temperature = 0,
      answer = None
    )
  )

}

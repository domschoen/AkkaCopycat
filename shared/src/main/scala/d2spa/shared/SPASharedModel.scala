package d2spa.shared

import boopickle.Default._
import boopickle.{MaterializePicklerFallback, TransformPicklers}
import d2spa.shared.WebSocketMessages._




object WebSocketMessages {

  // Client ---> Server
  // __________________

  sealed trait WebSocketMsgIn
  final case class Run(initialString: String, modifiedString: String, targetString: String) extends WebSocketMsgIn

  sealed trait WebSocketMsgOut

  final case class TemperatureMsg(temperature: Int) extends WebSocketMsgOut
  final case class AnswerMsg(value: String) extends WebSocketMsgOut
}







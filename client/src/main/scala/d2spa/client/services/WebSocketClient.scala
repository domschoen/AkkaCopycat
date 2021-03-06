package d2spa.client.services

import d2spa.client._
import d2spa.shared._
import d2spa.shared.WebSocketMessages._
import boopickle.Default._
import boopickle.{MaterializePicklerFallback, TransformPicklers}
import d2spa.client
import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.scalajs.js.timers._
import d2spa.client.logger._
import org.scalajs.dom.raw.MessageEvent

// google: scala.js websocket send java.nio.ByteBuffer
// ->
// Could be the solution:
// https://github.com/kiritsuku/amora/blob/master/web-ui/src/main/scala/amora/frontend/webui/Connection.scala

object WebSocketClient {
  val websocketUrl = s"ws://${dom.document.location.host}/ws"

  var socketOpt: Option[Socket] = None

  def setSocket() = {
    socketOpt = Some(Socket(websocketUrl)((event: MessageEvent) => event.data match {
      case blob: Blob =>
        //println("Will read socket")
        Socket.blobReader().readAsArrayBuffer(blob) //the callbacks in blobReader take care of what happens with the data.
      //Socket.blobReader.abort()
      case _ => dom.console.log("Error on receive, should be a blob.")
    }))

  }

  def send(msg: WebSocketMsgIn): Unit = {
    socketOpt match {
      case Some(socket) =>
        if (socket.isClosed) {
          setSocket()
        }
      case None =>
        setSocket()
    }
    socketOpt.get.send(msg)
  }


  case class Socket(url: String)(onMessage: (MessageEvent) => _) {
    println("  SOCKETE SOCKETE SOCKETE " + url)

    var isClosed = true
    var toBeSent: Option[WebSocketMsgIn] = None

    private val socket: WebSocket = new dom.WebSocket(url = url)

    def send(msg: WebSocketMsgIn): Unit = {
      if (isClosed) {
        toBeSent = Some(msg)
      } else {
        sendForSure(msg)
      }
    }


    def sendForSure(msg: WebSocketMsgIn): Unit = {
      import scala.scalajs.js.typedarray.TypedArrayBufferOps._
      val bytes = Pickle.intoBytes(msg).arrayBuffer()
      log.finest("Send " + msg)
      log.finest("Send " + bytes.byteLength + " bytes")
      socket.send(bytes)
    }


    socket.onopen = (e: Event) => {
      isClosed = false
    }
    socket.onclose = (e: CloseEvent) => {
        dom.console.log(s"Socket closed. Reason: ${e.reason} (${e.code})")
        isClosed = true
    }
    socket.onerror = (e: Event) => {
        dom.console.log(s"Socket error! ${e}")
    }
    socket.onmessage = onMessage
  }


  object Socket {
    def blobReader(): FileReader = {
      val reader = new FileReader()
      reader.onerror = (e: Event) => { dom.console.log(s"Error in blobReader: ${reader.error}") }
      reader.onload = (e: UIEvent) => {
        reader.result match {
          case buf: ArrayBuffer =>
            Unpickle[WebSocketMsgOut].fromBytes(TypedArrayBuffer.wrap(buf)) match {
              case TemperatureMsg(temperature) => MyCircuit.dispatch(SetTemperature(temperature))
              case AnswerMsg(answer) => MyCircuit.dispatch(SetAnswer(answer))
            }
          case _ => // ignored
        }
      }

      reader
    }
  }




}

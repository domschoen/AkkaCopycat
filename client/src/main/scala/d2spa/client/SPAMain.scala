package d2spa.client

import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import d2spa.client.components.GlobalStyles
import d2spa.client.logger._
import d2spa.client.services.{MyCircuit, WebSocketClient}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import CssSettings._
import scalacss.ScalaCssReact._
import d2spa.client.logger._
import d2spa.client.services.WebSocketClient.{Socket, websocketUrl}
import d2spa.shared._
import diode.react.ModelProxy
import org.scalajs.dom.{Blob, MessageEvent}
import org.scalajs.dom.raw.{ErrorEvent, Event, MessageEvent, WebSocket}

import scala.scalajs.js.JSApp

@JSExportTopLevel("SPAMain")
object SPAMain   extends  js.JSApp {
  sealed trait AppPage
  case object Home extends AppPage

    val routerConfig = RouterConfigDsl[AppPage].buildConfig { dsl =>
      import dsl._

      val menusConnection = MyCircuit.connect(_.content)


      (emptyRule
        | staticRoute(root, Home) ~> renderR(ctl => {
        menusConnection(p =>
          SVGTrial(p)
        )
      }
      )
        ).notFound(redirectToPage(Home)(Redirect.Replace))

    }


  @JSExport
  def main(): Unit = {

    // FINE level -> severe, warning, info

    //log.severe("Severe")
    //log.warning("warning")
    //log.info("info")
    //log.config("config")
    //log.fine("fine")
    //log.finer("finer")
    //log.finest("finest")

    // send log messages also to the server
    //log.enableServerLogging("/logging")
    log.info("This message goes to server as well")


    // create stylesheet
    GlobalStyles.addToDocument()

    //MyCircuit.dispatch(InitClient)

    val router = Router(BaseUrl.until_#, routerConfig)

    // tell React to render the router in the document body
    router().renderIntoDOM(dom.document.getElementById("root"))

  }



}

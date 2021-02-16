package d2spa.client

import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.{svg_<^ => svg}
import org.scalajs.dom.raw.{SVGGElement, SVGLocatable}
import diode.react.ModelProxy
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^.{<, ^, _}
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import diode.Action
import diode.data.Ready

import scala.scalajs.js


// https://reqres.in/
// https://github.com/andreaferretti/paths-scala-js-demo
// https://github.com/andreaferretti/paths-scala-js

object SimulationScreen {

  case class Props(proxy: ModelProxy[MegaContent])
  case class State(initialString: Option[String],
                   modifiedString: Option[String],
                   targetString: Option[String] )


  class Backend($ : BackendScope[Props, State]) {


    def simpleTextSvg() = {
      /*
      <?xml version="1.0" encoding="UTF-8"?>
      <svg width="250" height="100" viewBox="0 0 250 100" version="1.1" xmlns=”http://www.w3.org/2000/svg”>
      <text x="0" y="50" font-family="Verdana" font-size="50">
          Hello SVG
        </text>
      </svg>
       */
      svg.<.svg(
        svg.^.width := 250.asInstanceOf[JsNumber],
        svg.^.height := 100.asInstanceOf[JsNumber],
        svg.^.viewBox := "0 0 250 100",
        svg.<.text(
          svg.^.x := 0.asInstanceOf[JsNumber],
          svg.^.y := 50.asInstanceOf[JsNumber],
          ^.fontFamily := "Verdana",
          ^.fontSize := "50",
          "Hello SVG"
        )
      )
    }
    def handleSubmit(p: Props, s: State, e: ReactEventFromInput): Callback = {
      e.preventDefaultCB >> {
        p.proxy.dispatchCB(Run(s.initialString.get, s.modifiedString.get, s.targetString.get))
      }
    }

    def handleInitialStringChange(e: ReactEventFromInput) = {
      val newValue = if (e.target.value == null) None else Some(e.target.value)
      $.modState(_.copy(initialString = newValue))
    }
    def handleModifiedStringChange(e: ReactEventFromInput) = {
      val newValue = if (e.target.value == null) None else Some(e.target.value)
      $.modState(_.copy(modifiedString = newValue))
    }
    def handleTargetStringChange(e: ReactEventFromInput) = {
      val newValue = if (e.target.value == null) None else Some(e.target.value)
      $.modState(_.copy(targetString = newValue))
    }

    def render(p: Props, s:State):VdomElement  = {
      val initialString = s.initialString.getOrElse("")
      val modifiedString = s.modifiedString.getOrElse("")
      val targetString = s.targetString.getOrElse("")

      <.div(
        <.div(
          ^.className := "svgCont",
          <.form(^.className := "entriesForm", ^.onSubmit ==> { e: ReactEventFromInput => handleSubmit(p, s, e)},
            <.div(
              <.span(
                "Initial: ",
                <.input.text(^.placeholder := "Initial...", ^.value := initialString,
                  ^.onChange ==> { e: ReactEventFromInput => handleInitialStringChange(e)}),
                "Modified: ",
                <.input.text(^.placeholder := "Modified...", ^.value := modifiedString,
                  ^.onChange ==> { e: ReactEventFromInput => handleModifiedStringChange(e)}),
              )
            ),
            <.div(
              <.span(
                "Target: ",
                <.input.text(^.placeholder := "Target...", ^.value := targetString,
                  ^.onChange ==> { e: ReactEventFromInput => handleTargetStringChange(e)})
              )
            ),
            <.input.submit(^.value := "Run")
          ),
          <.div(^.className := "svgCont",
            d2spa.client.components.Thermometer(p.proxy.value.temperature)
          )
        )
      )
    }
  }




  private val component = ScalaComponent.builder[Props]("SimulationScreen")
    .initialState(State(Some("abc"),Some("abd"),Some("iijjkk")))
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[MegaContent]) = {
    component(Props(proxy))
  }
}

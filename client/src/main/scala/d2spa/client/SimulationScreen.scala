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
import scalajs.js
import js.UndefOr
import org.scalajs.jquery.jQuery
import com.highcharts.HighchartsUtils._
import com.highcharts.HighchartsAliases._
import com.highcharts.config._

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
                  ^.onChange ==> { e: ReactEventFromInput => handleTargetStringChange(e)}),
                "Answer: ",{
                  val answerOpt = p.proxy.value.answer
                  val answerText = if (answerOpt.isEmpty) "Processing..." else answerOpt.get
                  <.span(answerText)
                }
              )
            ),
            <.input.submit(^.value := "Run")
          ),
          <.div(^.className := "svgCont",
            d2spa.client.components.Thermometer(p.proxy.value.temperature)
          ),
          {
            //val container = js.Dynamic.literal(dom.document.getElementById("#container"))
            //val container = dom.document.getElementById("#container")


            jQuery("#container").highcharts(new HighchartsConfig {
              // Chart config
              override val chart: Cfg[Chart] = Chart(`type` = "bar")

              // Chart title
              override val title: Cfg[Title] = Title(text = "Demo bar chart")

              // X Axis settings
              override val xAxis: CfgArray[XAxis] = js.Array(XAxis(categories = js.Array("Apples", "Bananas", "Oranges")))

              // Y Axis settings
              override val yAxis: CfgArray[YAxis] = js.Array(YAxis(title = YAxisTitle(text = "Fruit eaten")))

              // Series
              override val series: SeriesCfg = js.Array[AnySeries](
                SeriesBar(name = "Jane", data = js.Array[Double](1, 0, 4)),
                SeriesBar(name = "John", data = js.Array[Double](5, 7, 3))
              )
            })
            <.div(^.id:="container")
          }
        )
      )
    }
  }




  private val component = ScalaComponent.builder[Props]("SimulationScreen")
    .initialState(State(Some("abc"),Some("abd"),Some("rssttt")))
    .renderBackend[Backend]
    .build

  def apply(proxy: ModelProxy[MegaContent]) = {
    component(Props(proxy))
  }
}

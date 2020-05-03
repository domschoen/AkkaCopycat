package d2spa.client.components
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

object Thermometer {

  private val component = ScalaComponent.builder[(Int)]("Thermometer")
    .render_P { case (heat) =>
      val ty = 500 - (10 * heat)
      val height = 50 + (10 * heat)
      svg.<.svg(
        svg.^.width := 100.asInstanceOf[JsNumber],
        svg.^.height := 424.5.asInstanceOf[JsNumber],
        svg.^.viewBox := "115.25 185 100 424.5",
        svg.<.g(
          // grey background circle on top
          svg.<.circle(
            svg.^.cx := 165.asInstanceOf[JsNumber],
            svg.^.cy := 215.asInstanceOf[JsNumber],
            svg.^.r := 30.asInstanceOf[JsNumber],
            svg.^.fill := "#d1d3d4"
          ),
          //<rect x="135" y="219.5" width="60" height="307.5" fill="#d1d3d4"/>
          // grey background bar
          svg.<.rect(
            svg.^.x := 135.asInstanceOf[JsNumber],
            svg.^.y := 219.5.asInstanceOf[JsNumber],
            svg.^.width := 60.asInstanceOf[JsNumber],
            svg.^.height := 307.5.asInstanceOf[JsNumber],
            svg.^.fill := "#d1d3d4"
          ),
          // grey background circle at bottom
          // <circle cx="165.25" cy="559.5" r="50.0000798950949" fill="#d1d3d4"/>
          svg.<.circle(
            svg.^.cx := 165.25.asInstanceOf[JsNumber],
            svg.^.cy := 559.5.asInstanceOf[JsNumber],
            svg.^.r := 50.asInstanceOf[JsNumber],
            svg.^.fill := "#d1d3d4"
          ),
          //         <circle cx="165.25" cy="559.5" r="45.0000719055854" fill="white"/>
          svg.<.circle(
            svg.^.cx := 165.25.asInstanceOf[JsNumber],
            svg.^.cy := 559.5.asInstanceOf[JsNumber],
            svg.^.r := 45.asInstanceOf[JsNumber],
            svg.^.fill := "white"
          ),
          //         <rect x="140" y="219.5" width="50" height="307.5" fill="white"/>
          svg.<.rect(
            svg.^.x := 140.asInstanceOf[JsNumber],
            svg.^.y := 219.5.asInstanceOf[JsNumber],
            svg.^.width := 50.asInstanceOf[JsNumber],
            svg.^.height := 307.5.asInstanceOf[JsNumber],
            svg.^.fill := "white"
          ),
          // <circle cx="165" cy="215" r="25.0000399475474" fill="white"/>
          svg.<.circle(
            svg.^.cx := 165.asInstanceOf[JsNumber],
            svg.^.cy := 215.asInstanceOf[JsNumber],
            svg.^.r := 25.asInstanceOf[JsNumber],
            svg.^.fill := "white"
          ),
          //  <circle cx="165.25" cy="559.5" r="27.5000439423022" fill="#ee3a43"/>
          svg.<.circle(
            svg.^.cx := 165.25.asInstanceOf[JsNumber],
            svg.^.cy := 559.5.asInstanceOf[JsNumber],
            svg.^.r := 27.5.asInstanceOf[JsNumber],
            svg.^.fill := "#ee3a43"
          ),
          //  <rect x="154" y="219.5" width="22.5" height="307.5" fill="#e6e7e8"/>
          svg.<.rect(
            svg.^.x := 154.asInstanceOf[JsNumber],
            svg.^.y := 219.5.asInstanceOf[JsNumber],
            svg.^.width := 22.5.asInstanceOf[JsNumber],
            svg.^.height := 307.5.asInstanceOf[JsNumber],
            svg.^.fill := "#e6e7e8"
          ),
          //  <circle cx="165.25" cy="215" r="11.2500179763964" fill="#e6e7e8"/>
          svg.<.circle(
            svg.^.cx := 165.25.asInstanceOf[JsNumber],
            svg.^.cy := 215.asInstanceOf[JsNumber],
            svg.^.r := 11.25.asInstanceOf[JsNumber],
            svg.^.fill := "#e6e7e8"
          ),
          //  <rect x="153.75" y="462.5" width="22.5" height="83" fill="#ee3a43"/>
          svg.<.rect(
            svg.^.x := 153.75.asInstanceOf[JsNumber],
            svg.^.y := ty.asInstanceOf[JsNumber],
            svg.^.width := 22.5.asInstanceOf[JsNumber],
            svg.^.height := height.asInstanceOf[JsNumber],
            svg.^.fill := "#ee3a43"
          ),

        )
      )
    }.build

  def apply(temperature: Int) = {
    component(temperature)
  }
}
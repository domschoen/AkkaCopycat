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

object SVGTrial {
  case class Stats(values: List[List[Double]], labels: List[String])
  case class Color(r: Double, g: Double, b: Double, alpha: Double = 1)

  val stats = Stats(
    values = List(
      List(1, 2.0, 3, 4),
      List(2, 3.0, 1, 4),
      List(2, 2.5, 3, 3)
    ),
    labels = List("2009", "2010", "2011", "2012")
  )

  def string(c: Color) =
    if (c.alpha == 1) s"rgb(${ c.r.floor },${ c.g.floor },${ c.b.floor })"
    else s"rgba(${ c.r.floor },${ c.g.floor },${ c.b.floor },${ c.alpha })"


  def cut(x: Double) = x.floor min 255

  def average(c1: Color, c2: Color) =
    Color(
      cut((c1.r + c2.r) / 2),
      cut((c1.g + c2.g) / 2),
      cut((c1.b + c2.b) / 2),
      (c1.alpha + c2.alpha / 2)
    )

  def multiply(factor: Double) = { c: Color =>
    Color(cut(factor * c.r), cut(factor * c.g), cut(factor * c.b), c.alpha)
  }


  val lighten = multiply(1.2)
  val darken = multiply(0.8)


  def mix(c1: Color, c2: Color) = {
    val c3 = average(c1, c2)
    val colors = List(
      lighten(c1),
      c1,
      darken(c1),
      lighten(c3),
      c3,
      darken(c3),
      lighten(c2),
      c2,
      darken(c2)
    )

    Stream.continually(colors).flatten
  }

  private val palette = mix(Color(130, 140, 210), Color(180, 205, 150))
  private def below(p: Array[Double]) = s"translate(${ p(0) }, 320)"


  case class Props(proxy: ModelProxy[MegaContent])


  class Backend($ : BackendScope[Props, Unit]) {
    def didMounted(p: Props) = {


      Callback.empty
    }




    private val Thermometer = ScalaComponent.builder[(Int)]("Thermometer")
      .render_P { case (heat) =>
        val tx = 500 - (10 * heat)
        svg.<.svg(
          svg.^.width := 100.asInstanceOf[JsNumber],
          svg.^.height := 424.5.asInstanceOf[JsNumber],
          svg.^.viewBox := "115.25 185 100 424.5",
          svg.<.g(
            // gray background
            svg.<.path(
              svg.^.d := "M552 3.5c-17.4 0-34.1 6.7-46 19.7H44.8c-22.5 0-41.3 17.3-41.3 39.3v4.8c0 10.7 4.7 21.4 12.5 29 7.7 7.6 18.1 12 28.8 12h461.7c11.8 11 28.2 18.2 45.5 18.2 35 0 63.5-27.6 63.5-61.4S587 3.5 552 3.5z",
              ^.className:="st0"
            ),
            // red rectangle for temperature
            svg.<.rect(
              ^.id:="thermometerLineBody",
              ^.`class` := "st1",
              svg.^.x := tx.asInstanceOf[JsNumber],
              svg.^.y := 42.asInstanceOf[JsNumber],
              svg.^.width := 500.asInstanceOf[JsNumber],
              svg.^.height := 42.asInstanceOf[JsNumber]
            ),
            // red circle at top
            svg.<.circle(^.id:= "thermometerLineHead",
              svg.^.cx := 40.asInstanceOf[JsNumber],
              svg.^.cy := 64.9.asInstanceOf[JsNumber],
              svg.^.r := 20.asInstanceOf[JsNumber],
              ^.`class` := "st1"
            ),
            // big white around
            svg.<.path(
              svg.^.d := "M552 3.3c-17.4 0-34.1 6.7-46 19.7H44.8C22.3 23 3.5 40.3 3.5 62.3v4.8c0 10.7 4.7 21.4 12.5 29 7.7 7.6 18.1 12 28.8 12h461.7c11.8 11 28.2 18.2 45.5 18.2 35 0 63.5-27.6 63.5-61.4S587 3.3 552 3.3zm0 98.1c-14.4 0-27-7.5-33.6-18.5H44.8c-8.4 0-15.8-7.1-15.8-15.5v-4.8c0-8.3 7.4-14.8 15.8-14.8H518c6.5-11 19.3-19.4 33.9-19.4 21.3 0 38.5 16.3 38.5 36.4.1 20.3-17.2 36.6-38.4 36.6z",
              ^.className:="st2"
            ),
            // glass tube (small grey stroke around)
            svg.<.path(
              svg.^.d := "M552 3.5c-17.4 0-34.1 6.7-46 19.7H44.8c-22.5 0-41.3 17.3-41.3 39.3v4.8c0 10.7 4.7 21.4 12.5 29 7.7 7.6 18.1 12 28.8 12h461.7c11.8 11 28.2 18.2 45.5 18.2 35 0 63.5-27.6 63.5-61.4S587 3.5 552 3.5z",
              ^.className:="st3"
            )
          )
        )
      }.build

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
    def changeTemperature() = {
      //Callback.log(s"Import") >>
      $.props >>= (_.proxy.dispatchCB(ChangeTemperature))
    }

    def render(p: Props):VdomElement  = {
      <.div(
        <.div(
          ^.className := "svgCont",
          <.button(^.className := "btn btn-primary", ^.onClick --> changeTemperature(),"Action")
        ),
        <.div(^.className := "svgCont",
          simpleTextSvg()
        ),
      //renderBarChar()
      //renderSimpleSvg()
        <.div(^.className := "svgCont",
          Thermometer(p.proxy.value.temperature)
        )
      )
    }
  }




  private val component = ScalaComponent.builder[Props]("D3Trial")
    .renderBackend[Backend]
    .componentDidMount(scope => scope.backend.didMounted(scope.props))
    .build

  def apply(proxy: ModelProxy[MegaContent]) = {
    component(Props(proxy))
  }
}

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
      $.props >>= (_.proxy.dispatchCB(d2spa.client.SetTemperature(4)))
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
          d2spa.client.components.Thermometer(p.proxy.value.temperature)
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

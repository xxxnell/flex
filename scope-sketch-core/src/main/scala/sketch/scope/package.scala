package sketch

import cats.data.Kleisli
import sketch.scope.conf.CustomSketchConf
import sketch.scope.measure.TrivialMeasures
import sketch.scope.pdf.syntax.{DistSyntax, SamplingDistSyntax, SketchSyntax, SmoothDistSyntax}
import sketch.scope.plot.{CountPlotSyntax, DensityPlotSyntax, PlotSyntax}
import sketch.scope.range.{RangeMSyntax, RangePSyntax}
import sketch.scope.sim.SimSyntax

package object scope
  extends ConfPkgSyntax
    with MeasurePkgSyntax
    with PdfPkgSyntax
    with PlotPkgSyntax
    with RangePkgSyntax
    with SimPkgSyntax {

  type Mon[A, B] = Kleisli[Some, A, B]

  type Epi[A, B] = Kleisli[Option, A, B]

}

trait ConfPkgSyntax {

  implicit val defaultSketchConf: sketch.scope.conf.SketchConf = sketch.scope.conf.SketchConf.default

  type SketchConf = CustomSketchConf

  val SketchConf: CustomSketchConf.type = CustomSketchConf

}

trait MeasurePkgSyntax
  extends TrivialMeasures

trait PdfPkgSyntax
  extends DistSyntax
    with SamplingDistSyntax
    with SmoothDistSyntax
    with SketchSyntax {

  type Dist[A] = sketch.scope.pdf.Dist[A]

  val Dist: sketch.scope.pdf.Dist.type = sketch.scope.pdf.Dist

  type Sketch[A] = sketch.scope.pdf.Sketch[A]

  val Sketch: sketch.scope.pdf.Sketch.type = sketch.scope.pdf.Sketch

}

trait PlotPkgSyntax
  extends PlotSyntax
    with DensityPlotSyntax
    with CountPlotSyntax {

  type CountPlot = sketch.scope.plot.CountPlot

  type DensityPlot = sketch.scope.plot.DensityPlot

}

trait RangePkgSyntax
  extends RangePSyntax
    with RangeMSyntax

trait SimPkgSyntax
  extends SimSyntax

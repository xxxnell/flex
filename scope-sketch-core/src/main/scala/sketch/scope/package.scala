package sketch.scope

import cats.data.Kleisli
import sketch.scope.conf.CustomSketchConf
import sketch.scope.measure.TrivialMeasures
import sketch.scope.pdf.syntax.{DistSyntax, SamplingDistSyntax, SketchSyntax, SmoothDistSyntax}
import sketch.scope.plot.{CountPlotSyntax, DensityPlotSyntax, PlotSyntax}
import sketch.scope.range.{RangeMSyntax, RangePSyntax}

/**
  * Licensed by Probe Technology, Inc.
  */
object `package`
  extends ConfPkgSyntax
    with MeasurePkgSyntax
    with PdfPkgSyntax
    with PlotPkgSyntax
    with RangePkgSyntax {

  type Mon[A, B] = Kleisli[Some, A, B]

  type Epi[A, B] = Kleisli[Option, A, B]

}

trait ConfPkgSyntax {

  implicit val defaultSketchConf: conf.SketchConf = conf.SketchConf.default

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

  type Dist[A] = pdf.Dist[A]

  val Dist: pdf.Dist.type = pdf.Dist

  type Sketch[A] = pdf.Sketch[A]

  val Sketch: pdf.Sketch.type = pdf.Sketch

}

trait PlotPkgSyntax
  extends PlotSyntax
    with DensityPlotSyntax
    with CountPlotSyntax

trait RangePkgSyntax
  extends RangePSyntax
    with RangeMSyntax


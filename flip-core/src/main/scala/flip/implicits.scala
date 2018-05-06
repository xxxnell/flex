package flip

import flip.conf.{CustomSimpleSketchConf, CustomSketchConf, SimpleSketchConf, SmoothDistConf}
import flip.measure.TrivialMeasures
import flip.pdf.SimpleSketch
import flip.pdf.syntax.{DistSyntax, NumericDistSyntax, SamplingDistSyntax, SketchSyntax, SmoothDistSyntax}
import flip.plot.{CountPlotSyntax, DensityPlotSyntax, PlotSyntax, RangePlotSyntax}
import flip.range.RangeSyntax
import flip.sim.SimSyntax

object implicits extends AllSyntax

trait AllSyntax
    extends ConfPkgSyntax
    with MeasurePkgSyntax
    with PdfPkgSyntax
    with PlotPkgSyntax
    with RangePkgSyntax
    with SimPkgSyntax

trait ConfPkgSyntax extends ConfPkgSyntax1 {

  type SketchConf = flip.conf.CustomSketchConf

  def SketchConf: CustomSketchConf.type = CustomSketchConf

  type HistogramConf = flip.conf.CustomSimpleSketchConf

  def HistogramConf: CustomSimpleSketchConf.type = CustomSimpleSketchConf

}

trait ConfPkgSyntax1 extends ConfPkgSyntax2 {

  implicit val defaultSmoothDistConf: flip.conf.SmoothDistConf = SmoothDistConf.default

}

trait ConfPkgSyntax2 {

  implicit val defaultSketchConf: flip.conf.SketchConf = flip.conf.SketchConf.default

}

trait MeasurePkgSyntax extends TrivialMeasures

trait PdfPkgSyntax
    extends DistSyntax
    with SamplingDistSyntax
    with SmoothDistSyntax
    with SketchSyntax
    with NumericDistSyntax {

  type Dist[A] = flip.pdf.Dist[A]

  val Dist: flip.pdf.Dist.type = flip.pdf.Dist

  type NumericDist[A] = flip.pdf.NumericDist[A]

  val NumericDist: flip.pdf.NumericDist.type = flip.pdf.NumericDist

  type Sketch[A] = flip.pdf.Sketch[A]

  val Sketch: flip.pdf.Sketch.type = flip.pdf.Sketch

  type Histogram[A] = flip.pdf.SimpleSketch[A]

  val Histogram: SimpleSketch.type = flip.pdf.SimpleSketch

}

trait PlotPkgSyntax extends flip.plot.PlotPkgSyntax {

  type CountPlot = flip.plot.CountPlot

  type DensityPlot = flip.plot.DensityPlot

  type PointPlot = flip.plot.PointPlot

}

trait RangePkgSyntax extends RangeSyntax

trait SimPkgSyntax extends SimSyntax

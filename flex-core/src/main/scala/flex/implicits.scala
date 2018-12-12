package flex

import flex.conf.pdf._
import flex.measure.TrivialMeasures
import flex.pdf.syntax.{
  DataBinningDistSyntax,
  DistSyntax,
  NumericDistSyntax,
  SamplingDistSyntax,
  SketchSyntax,
  SmoothDistSyntax
}
import flex.range.RangeSyntax
import flex.sim.SimSyntax

object implicits extends AllSyntax

trait AllSyntax
    extends ConfPkgSyntax
    with MeasurePkgSyntax
    with PdfPkgSyntax
    with PlotPkgSyntax
    with RangePkgSyntax
    with SimPkgSyntax

trait ConfPkgSyntax extends flex.conf.ConfPkgSyntax {

  type SketchConf = CustomSketchConf

  def SketchConf: CustomAdaSelSketchConf.type = CustomAdaSelSketchConf

}

trait MeasurePkgSyntax extends TrivialMeasures

trait PdfPkgSyntax extends flex.pdf.syntax.PdfPkgSyntax {

  type Dist[A] = flex.pdf.Dist[A]

  val Dist: flex.pdf.Dist.type = flex.pdf.Dist

  type NumericDist[A] = flex.pdf.NumericDist[A]

  val NumericDist: flex.pdf.NumericDist.type = flex.pdf.NumericDist

  type Sketch[A] = flex.pdf.Sketch[A]

  val Sketch: flex.pdf.Sketch.type = flex.pdf.Sketch

}

trait PlotPkgSyntax extends flex.plot.PlotPkgSyntax {

  type CountPlot = flex.plot.CountPlot

  type DensityPlot = flex.plot.DensityPlot

  type PointPlot = flex.plot.PointPlot

}

trait RangePkgSyntax extends RangeSyntax

trait SimPkgSyntax extends SimSyntax

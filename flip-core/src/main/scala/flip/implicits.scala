package flip

import flip.conf.pdf._
import flip.measure.TrivialMeasures
import flip.pdf.syntax.{
  DataBinningDistSyntax,
  DistSyntax,
  NumericDistSyntax,
  SamplingDistSyntax,
  SketchSyntax,
  SmoothDistSyntax
}
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

trait ConfPkgSyntax extends flip.conf.ConfPkgSyntax {

  type SketchConf = CustomSketchConf

  def SketchConf: CustomAdaSelSketchConf.type = CustomAdaSelSketchConf

}

trait MeasurePkgSyntax extends TrivialMeasures

trait PdfPkgSyntax extends flip.pdf.syntax.PdfPkgSyntax {

  type Dist[A] = flip.pdf.Dist[A]

  val Dist: flip.pdf.Dist.type = flip.pdf.Dist

  type NumericDist[A] = flip.pdf.NumericDist[A]

  val NumericDist: flip.pdf.NumericDist.type = flip.pdf.NumericDist

  type Sketch[A] = flip.pdf.Sketch[A]

  val Sketch: flip.pdf.Sketch.type = flip.pdf.Sketch

}

trait PlotPkgSyntax extends flip.plot.PlotPkgSyntax {

  type CountPlot = flip.plot.CountPlot

  type DensityPlot = flip.plot.DensityPlot

  type PointPlot = flip.plot.PointPlot

}

trait RangePkgSyntax extends RangeSyntax

trait SimPkgSyntax extends SimSyntax

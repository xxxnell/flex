import cats.data.Kleisli
import flip.conf.{CustomSimpleSketchConf, CustomSketchConf, SimpleSketchConf, SmoothDistConf}
import flip.measure.TrivialMeasures
import flip.pdf.SimpleSketch
import flip.pdf.syntax.{DistSyntax, SamplingDistSyntax, SketchSyntax, SmoothDistSyntax}
import flip.plot.{CountPlotSyntax, DensityPlotSyntax, PlotSyntax}
import flip.range.RangeSyntax
import flip.sim.SimSyntax

package object flip
  extends ConfPkgSyntax
    with MeasurePkgSyntax
    with PdfPkgSyntax
    with PlotPkgSyntax
    with RangePkgSyntax
    with SimPkgSyntax {

  type Mon[A, B] = Kleisli[Some, A, B]

  type Epi[A, B] = Kleisli[Option, A, B]

  def time[R](block: => R, tag: String = "", display: Boolean = true): R =
    timePrint(block, if(tag.isEmpty) None else Some(tag), display)

  def timePrint[R](block: => R, tag: Option[String], display: Boolean): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val tagPrefixed = tag.map(s => s" $s")
    if(display) println(s"Elapsed time${tagPrefixed.getOrElse("")}: " + (t1 - t0) + " ns")
    result
  }

  def timeCost[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

}

trait ConfPkgSyntax extends ConfPkgSyntax1 {

  type SketchConf = CustomSketchConf

  val SketchConf: CustomSketchConf.type = CustomSketchConf

  type HistogramConf = CustomSimpleSketchConf

  def HistogramConf: SimpleSketchConf.type = SimpleSketchConf

}

trait ConfPkgSyntax1 extends ConfPkgSyntax2 {

  implicit val defaultSmoothDistConf: flip.conf.SmoothDistConf = SmoothDistConf.default

}

trait ConfPkgSyntax2 {

  implicit val defaultSketchConf: flip.conf.SketchConf = flip.conf.SketchConf.default

}

trait MeasurePkgSyntax
  extends TrivialMeasures

trait PdfPkgSyntax
  extends DistSyntax
    with SamplingDistSyntax
    with SmoothDistSyntax
    with SketchSyntax {

  type Dist[A] = flip.pdf.Dist[A]

  val Dist: flip.pdf.Dist.type = flip.pdf.Dist

  val NumericDist: flip.pdf.NumericDist.type = flip.pdf.NumericDist

  type Sketch[A] = flip.pdf.Sketch[A]

  val Sketch: flip.pdf.Sketch.type = flip.pdf.Sketch

  type Histogram[A] = flip.pdf.SimpleSketch[A]

  val Histogram: SimpleSketch.type = flip.pdf.SimpleSketch

}

trait PlotPkgSyntax
  extends PlotSyntax
    with DensityPlotSyntax
    with CountPlotSyntax {

  type CountPlot = flip.plot.CountPlot

  type DensityPlot = flip.plot.DensityPlot

}

trait RangePkgSyntax
  extends RangeSyntax

trait SimPkgSyntax
  extends SimSyntax

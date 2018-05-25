package flip.plot

import cats.data.NonEmptyList
import flip.range.syntax._
import flip.plot.syntax._

trait PlotPkgSyntax
    extends PlotSyntax
    with PointPlotSyntax
    with RangePlotSyntax
    with CountPlotSyntax
    with DensityPlotSyntax

trait PlotSyntax {

  type Record = (RangeP, Double)

  implicit class PlotSyntaxImpl(plot: Plot) {
    def interpolation(argument: Double): Double = Plot.interpolation(plot, argument)
    def of(argument: Double): Double = Plot.interpolation(plot, argument)
  }

}

trait PolyPlotSyntax[P <: Plot] {
  // context
  def plot: P
  def ops: PlotOps[P]
  // syntax
  def add(plot2: P): P = ops.add(NonEmptyList.of((1.0, plot), (1.0, plot2)))
  def :+(plot2: P): P = add(plot2)
  def inverse: P = ops.inverse(plot)
  def normalizedCumulative: P = ops.normalizedCumulative(plot)
  def inverseNormalizedCumulative: P = ops.inverseNormalizedCumulative(plot)
}

trait PointPlotSyntax {

  implicit class PointPlotSyntaxImpl(pPlot: PointPlot) extends PolyPlotSyntax[PointPlot] {
    def plot: PointPlot = pPlot
    def ops: PointPlotOps[PointPlot] = PointPlot
    def integralAll: Double = PointPlot.integralAll(plot)
    def avgChangeRate: PointPlot = PointPlot.avgChangeRate(pPlot)
    def map(f: (Double, Double) => (Double, Double)): PointPlot = PointPlot.map(pPlot, f)
    def referencialInterpolation(x: Double, ref: Int): Option[Double] =
      PointPlot.referencialInterpolation(pPlot, x, ref)
    def csv: String = ShowPlot.dsvPointPlot(plot, ", ")
    def dsv(delimiter: String): String = ShowPlot.dsvPointPlot(plot, delimiter)
  }

  implicit class DensityPlotAddSyntaxImpl(wp: (Double, PointPlot)) {
    def :+(wp2: (Double, PointPlot)): PointPlot = PointPlot.add(NonEmptyList.of(wp, wp2))
  }

}

trait RangePlotSyntax {

  implicit class RangePlotSyntaxImpl(plot: RangePlot) {
    def image(argument: Double): Option[Double] = RangePlot.image(plot, argument)
    def value(argument: Double): Option[Double] = RangePlot.image(plot, argument)
    def integral(start: Double, end: Double): Double = RangePlot.integral(plot, start, end)
    def integralAll: Double = RangePlot.integralAll(plot)
    def isEmpty: Boolean = RangePlot.isEmpty(plot)
    def nonEmpty: Boolean = !RangePlot.isEmpty(plot)
    def csv: String = ShowPlot.dsvRangePlot(plot, ", ")
    def dsv(delimiter: String): String = ShowPlot.dsvRangePlot(plot, delimiter)
  }

}

trait PolyRangePlotSyntax[P <: RangePlot] extends PolyPlotSyntax[P] {
  // context
  def plot: P
  def ops: RangePlotOps[P]
  // syntax
  def modify(f: Record => Double): P = ops.modifyValue(plot, f)
  def ++(plot2: P): P = ops.concat(plot, plot2)
  def multiply(mag: Double): P = ops.multiplyConstant(plot, mag)
  def *(mag: Double): P = ops.multiplyConstant(plot, mag)
  def domain: Option[RangeP] = ops.domain(plot)
}

trait CountPlotSyntax {

  implicit class CountPlotSyntaxImpl(cPlot: CountPlot) extends PolyRangePlotSyntax[CountPlot] {
    def plot: CountPlot = cPlot
    def ops: RangePlotOps[CountPlot] = CountPlot
  }

}

trait DensityPlotSyntax {

  implicit class DensityPlotSyntaxImpl(dPlot: DensityPlot) extends PolyRangePlotSyntax[DensityPlot] {
    def plot: DensityPlot = dPlot
    def ops: RangePlotOps[DensityPlot] = DensityPlot
  }

  implicit class DensityPlotAddSyntaxImpl(wp: (Double, DensityPlot)) {
    def :+(wp2: (Double, DensityPlot)): DensityPlot = DensityPlot.add(NonEmptyList.of(wp, wp2))
  }

}

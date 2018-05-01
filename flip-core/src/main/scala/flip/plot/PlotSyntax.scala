package flip.plot

import cats.data.NonEmptyList
import flip.range.syntax._
import flip.plot.syntax._

trait PlotSyntax {

  type Record = (RangeP, Double)

  implicit class PlotSyntaxImpl(plot: Plot) {
    def interpolation(argument: Double): Double = Plot.interpolation(plot, argument)
  }

}

trait PolyPlotSyntax[P <: Plot] {
  // context
  def plot: P
  def ops: PlotOps[P]
  // syntax
  def add(plot2: P): P = ops.add(plot, plot2)
  def +(plot2: P): P = ops.add(plot, plot2)
  def inverse: P = ops.inverse(plot)
  def normalizedCumulative: P = ops.normalizedCumulative(plot)
  def inverseNormalizedCumulative: P = ops.inverseNormalizedCumulative(plot)
}

trait RangePlotSyntax {

  implicit class RangePlotSyntaxImpl(plot: RangePlot) {
    def image(argument: Double): Option[Double] = RangePlot.image(plot, argument)
    def value(argument: Double): Option[Double] = RangePlot.image(plot, argument)
    def integral(start: Double, end: Double): Double = RangePlot.integral(plot, start, end)
    def integralAll: Double = RangePlot.integralAll(plot)
    def isEmpty: Boolean = RangePlot.isEmpty(plot)
    def nonEmpty: Boolean = !RangePlot.isEmpty(plot)
    def csv: String = ShowPlot.dsv(plot, ", ")
    def dsv(delimiter: String): String = ShowPlot.dsv(plot, delimiter)
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
    def ++(wp2: (Double, DensityPlot)): DensityPlot = DensityPlot.weightedAdd(NonEmptyList.of(wp, wp2))
  }

}

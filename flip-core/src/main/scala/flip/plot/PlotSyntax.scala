package flip.plot

import flip.range.syntax._
import flip.plot.syntax._

trait PlotSyntax {

  type Record = (RangeP, Double)

  implicit class PlotSyntaxImpl(plot: Plot) {
    def image(argument: Double): Option[Double] = Plot.image(plot, argument)
    def value(argument: Double): Option[Double] = Plot.image(plot, argument)
    def interpolation(argument: Double): Double = Plot.interpolation(plot, argument)
    def integral(start: Double, end: Double): Double = Plot.integral(plot, start, end)
    def integralAll: Double = Plot.integralAll(plot)
    def isEmpty: Boolean = Plot.isEmpty(plot)
    def nonEmpty: Boolean = !Plot.isEmpty(plot)
    def csv: String = ShowPlot.dsv(plot, ", ")
    def dsv(delimiter: String): String = ShowPlot.dsv(plot, delimiter)
  }

}

trait PolyPlotSyntax[P<:Plot] {
  // context
  def plot: P
  def ops: PlotOps[P]
  // syntax
  def modify(f: Record => Double): P = ops.modifyValue(plot, f)
  def add(plot2: P): P = ops.add(plot, plot2)
  def +(plot2: P): P = ops.add(plot, plot2)
  def multiply(mag: Double): P = ops.multiplyConstant(plot, mag)
  def *(mag: Double): P = ops.multiplyConstant(plot, mag)
  def inverse: P = ops.inverse(plot)
  def domain: Option[RangeP] = ops.domain(plot)
}

trait CountPlotSyntax {

  implicit class CountPlotSyntaxImpl(cPlot: CountPlot) extends PolyPlotSyntax[CountPlot] {
    def plot: CountPlot = cPlot
    def ops: PlotOps[CountPlot] = CountPlot
  }

}

trait DensityPlotSyntax {

  implicit class DensityPlotSyntaxImpl(dPlot: DensityPlot) extends PolyPlotSyntax[DensityPlot] {
    def plot: DensityPlot = dPlot
    def ops: PlotOps[DensityPlot] = DensityPlot

    def cumulative: DensityPlot = DensityPlot.cumulative(plot)
    def inverseCumulative: DensityPlot = DensityPlot.inverseCumulative(plot)
  }

}


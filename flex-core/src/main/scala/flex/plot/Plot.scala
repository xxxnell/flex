package flex.plot

import cats.data.NonEmptyList

trait Plot

trait PlotOps[P <: Plot] {

  def interpolation(plot: P, x: Double): Double

  def add(plots: NonEmptyList[(Double, P)]): P

  def inverse(plot: P): P

  def normalizedCumulative(plot: P): P

  def inverseNormalizedCumulative(plot: P): P

}

object Plot extends PlotOps[Plot] {

  def interpolation(plot: Plot, x: Double): Double = plot match {
    case plot: PointPlot => PointPlot.interpolation(plot, x)
    case plot: RangePlot => RangePlot.interpolation(plot, x)
  }

  def add(plots: NonEmptyList[(Double, Plot)]): Plot = plots match {
    case plots: NonEmptyList[(Double, PointPlot)] => PointPlot.add(plots)
    case plots: NonEmptyList[(Double, RangePlot)] => RangePlot.add(plots)
  }

  def inverse(plot: Plot): Plot = plot match {
    case plot: PointPlot => PointPlot.inverse(plot)
    case plot: RangePlot => RangePlot.inverse(plot)
  }

  def normalizedCumulative(plot: Plot): Plot = plot match {
    case plot: PointPlot => PointPlot.normalizedCumulative(plot)
    case plot: RangePlot => RangePlot.normalizedCumulative(plot)
  }

  def inverseNormalizedCumulative(plot: Plot): Plot = plot match {
    case plot: PointPlot => PointPlot.inverseNormalizedCumulative(plot)
    case plot: RangePlot => RangePlot.inverseNormalizedCumulative(plot)
  }

}

package flip.plot

import cats.data.NonEmptyList

trait PointPlot extends Plot {

  def records: Array[Array[Double]]

}

trait PointPlotOps[P <: PointPlot] extends PlotOps[P] {

  def interpolation(plot: P, x: Double): Double = ???

  def add(plots: NonEmptyList[(Double, P)]): P = ???

  def inverse(plot: P): P = ???

  def normalizedCumulative(plot: P): P = ???

  def inverseNormalizedCumulative(plot: P): P = ???

}

object PointPlot extends PointPlotOps[PointPlot] {

  private case class PointPlotImpl(records: Array[Array[Double]]) extends PointPlot

  def apply(records: Array[Array[Double]]): PointPlot = ???

}

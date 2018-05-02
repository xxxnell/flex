package flip.plot

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

  def interpolation(plot: Plot, x: Double): Double = ???

  def add(plots: NonEmptyList[(Double, Plot)]): Plot = ???

  def inverse(plot: Plot): Plot = ???

  def normalizedCumulative(plot: Plot): Plot = ???

  def inverseNormalizedCumulative(plot: Plot): Plot = ???

}

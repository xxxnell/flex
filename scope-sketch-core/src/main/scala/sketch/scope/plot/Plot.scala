package sketch.scope.plot

import sketch.scope.range._

/**
  * Licensed by Probe Technology, Inc.
  *
  * Disjoint set of range and value
  */
trait Plot {

  def records: List[Record]

}

trait PlotOps extends PlotLaws {

  def modify(plot: Plot, f: Record => Double): Plot

  def inverse(plot: Plot): Plot

  def cumulative(plot: Plot): Plot

  def image(plot: Plot, argument: Double): Double

}

trait PlotLaws { self: PlotOps =>

  def add(plot1: Plot, plot2: Plot): Plot = ???

  def multiply(plot: Plot, mag: Double): Plot = ???

}

trait PlotSyntax {

  implicit class PlotSyntaxImpl(plot: Plot) {
    def modify(f: Record => Double): Plot = Plot.modify(plot, f)
    def add(plot2: Plot): Plot = Plot.add(plot, plot2)
    def +(plot2: Plot): Plot = Plot.add(plot, plot2)
    def multiply(mag: Double): Plot = Plot.multiply(plot, mag)
    def *(mag: Double): Plot = Plot.multiply(plot, mag)
    def inverse: Plot = Plot.inverse(plot)
    def cumulative: Plot = Plot.cumulative(plot)
    def image(argument: Double): Double = Plot.image(plot, argument)
    def value(argument: Double): Double = Plot.image(plot, argument)
  }

}

object Plot extends PlotOps {

  def apply(records: List[Record]): Plot = disjoint(records)

  def disjoint(records: List[Record]): Plot = ???

  def squareKernel(ds: List[Double], window: Double): Plot =
    disjoint(ds.map(d => (Range(d - (window / 2), d + (window / 2)), 1 / window)))

  // ops

  def modify(plot: Plot, f: Record => Double): Plot = ???

  def inverse(plot: Plot): Plot = ???

  def cumulative(plot: Plot): Plot = ???

  def image(plot: Plot, argument: Double): Double = ???

}
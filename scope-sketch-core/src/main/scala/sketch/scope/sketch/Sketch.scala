package sketch.scope.sketch

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import cats.implicits._

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch {

//  def measure: A => Double

  def structure: List[(Cmap, HCounter)]

}

trait SketchOps[S<:Sketch] extends SketchLaws[S] {

  def primitiveUpdate(sketch: S, p: Double): Option[S]

  def primitiveCount(sketch: S, pFrom: Double, pTo: Double): Option[Double]

  /**
    * Total number of elements be memorized.
    * @return
    * */
  def sum(sketch: S): Double

  /**
    * Clear all memorization.
    * @return
    * */
//  def clear(sketch: S): S

  def rearrange(sketch: S): Option[S]

}

trait SketchLaws[S<:Sketch] { self: SketchOps[S] =>

  /**
    * Update the element to be memorized.
    * @return
    * */
//  def update[A: TypeTag](sketch: Sketch[A], a: A): Option[Sketch[A]] = ???
//    primitiveUpdate(sketch, sketch.measure(a))

  /**
    * Get the number of elements be memorized.
    * @return
    * */
//  def count[A: TypeTag](sketch: S[A], from: A, to: A): Double = ???
//    primitiveCount(sketch, sketch.measure(from), sketch.measure(to))

  /**
    * @return
    * */
  def probability(sketch: S, from: Double, to: Double): Option[Double] =
    primitiveCount(sketch, from, to)
      .map(count => (BigDecimal(count) / BigDecimal(sum(sketch))).toDouble)

  /**
    * @return
    * */
//  def pdf(sketch: S, a: Double): Option[Double] = ???

  /**
    * @return
    * */
//  def cdf(sketch: S, a: Double): Option[Double] = ???

  def plot(sketch: S): Option[List[(Range, Double)]] = {
    for {
      cmapHcounter <- sketch.structure.lastOption
      (cmap, _) = cmapHcounter
      ranges = cmap.bin
      counts <- ranges.traverse(range => primitiveCount(sketch, range.start, range.end))
    } yield ranges.zip(counts)
  }

  def densityPlot(sketch: S): Option[List[(Range, Double)]] = {
    val sum = self.sum(sketch)
    plot(sketch).map(plot =>
      plot.map { case (range, count) => (range, count / (sum * (range.end - range.start))) }
    )
  }

}

trait SketchSyntax {

  implicit class SketchSyntaxImpl(sketch: Sketch) {
    def update(a: Double): Option[Sketch] = Sketch.primitiveUpdate(sketch, a)
    def count(from: Double, to: Double): Option[Double] = Sketch.primitiveCount(sketch, from, to)
    def sum: Double = Sketch.sum(sketch)
//    def clear: Sketch = Sketch.clear(sketch)
    def probability(from: Double, to: Double): Option[Double] = Sketch.probability(sketch, from, to)
    def rearrange: Option[Sketch] = Sketch.rearrange(sketch)
  }

}

object Sketch extends SketchOps[Sketch] {

  def empty(caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): Sketch =
    PeriodicSketch.empty(caDepth, caSize, coDepth, coSize)

  def primitiveUpdate(sketch: Sketch, p: Double): Option[Sketch] = sketch match {
    case sketch: PeriodicSketch => PeriodicSketch.primitiveUpdate(sketch, p)
  }

  def primitiveCount(sketch: Sketch, pFrom: Double, pTo: Double): Option[Double] = sketch match {
    case sketch: PeriodicSketch => PeriodicSketch.primitiveCount(sketch, pFrom, pTo)
  }

  def sum(sketch: Sketch): Double = sketch match {
    case sketch: PeriodicSketch => PeriodicSketch.sum(sketch)
  }

//  def clear(sketch: Sketch): Sketch = sketch match {
//    case sketch: PeriodicSketch => PeriodicSketch.clear(sketch)
//  }

  def rearrange(sketch: Sketch): Option[Sketch] = sketch match {
    case sketch: PeriodicSketch => PeriodicSketch.rearrange(sketch)
  }

}
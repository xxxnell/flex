package sketch.scope.sketch

import cats.implicits._
import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.HDim

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A] extends Dist[A] {

  def measure: A => Prim

  def structure: Structure

}

trait SketchOps[S[_]<:Sketch[_]] extends SketchLaws[S] with DistOps[S] {

  def modifyStructure[A](sketch: S[A], f: Structure => Option[Structure]): Option[S[A]]

  /**
    * Update a primitive value <code>p</code> without rearrange process.
    * */
  def simpleUpdate[A](sketch: S[A], p: Prim): Option[S[A]] = modifyStructure(sketch, str =>
    str.traverse { case (cmap, hcounter) => hcounter.update(cmap.apply(p), 1).map(hcounter => (cmap, hcounter)) } )

  /**
    * Update a primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primUpdate[A](sketch: S[A], p: Prim): Option[S[A]]

  def singleCount(cmap: Cmap, hcounter: HCounter, pFrom: Double, pTo: Double): Option[Double] = {
    val (fromHdim, toHdim) = (cmap.apply(pFrom), cmap.apply(pTo))
    val (fromRng, toRng) = (cmap.range(fromHdim), cmap.range(toHdim))

    // mid count
    val midRangeO: Option[(HDim, HDim)] = if((toHdim-1) > (fromHdim+1)) {
      Some((fromHdim + 1, toHdim - 1))
    } else None
    val midCountO = midRangeO.fold(Option(0.0)){ case (from, to) => hcounter.count(from, to) }

    // from count
    val fromDensityO = hcounter.count(fromHdim, fromHdim).map(c => c / (fromRng.end - fromRng.start))
    val fromCountO = fromDensityO.map(density => (fromRng.end - pFrom) * density)

    // to count
    val toDensityO = hcounter.count(toHdim, toHdim).map(c => c / (toRng.end - toRng.start))
    val toCountO = toDensityO.map(density => (pTo - toRng.start) * density)

    for {
      toCount <- toCountO
      fromCount <- fromCountO
      midCount <- midCountO
    } yield toCount + fromCount + midCount
  }

  def primCount(sketch: S[_], pFrom: Prim, pTo: Prim): Option[Double] = {
    val countsO = sketch.structure.traverse { case (cmap, hcounter) => singleCount(cmap, hcounter, pFrom, pTo) }
    countsO.map(counts => counts.sum / counts.size)
  }

  /**
    * Total number of elements be memorized.
    * */
  def sum(sketch: S[_]): Double = {
    val sums = sketch.structure.map { case (_, hcounter) => hcounter.sum }
    sums.sum / sums.size
  }

  /**
    * Clear all memorization.
    * */
//  def clear(sketch: S): S

  def rearrange[A](sketch: S[A]): Option[S[A]]

}

trait SketchLaws[S[_]<:Sketch[_]] { self: SketchOps[S] =>

  /**
    * Update the element to be memorized.
    * */
  def update[A](sketch: S[A], a: A): Option[S[A]] = {
    primUpdate(sketch, sketch.measure.asInstanceOf[A => Double](a))
  }

  /**
    * Get the number of elements be memorized.
    * */
  def count[A](sketch: S[A], from: A, to: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[A => Double]
    primCount(sketch, measure(from), measure(to))
  }

  /***/
  def probability[A](sketch: S[A], from: A, to: A): Option[Double] = for {
    count <- count(sketch, from, to)
    sum = self.sum(sketch)
  } yield (BigDecimal(count) / BigDecimal(sum)).toDouble

//  def pdf(sketch: S, a: Double): Option[Double] = ???

//  def cdf(sketch: S, a: Double): Option[Double] = ???

  def countPlot(sketch: S[_]): Option[List[(Range, Double)]] = {
    for {
      cmapHcounter <- sketch.structure.lastOption
      (cmap, _) = cmapHcounter
      ranges = cmap.bin
      counts <- ranges.traverse(range => primCount(sketch, range.start, range.end))
    } yield ranges.zip(counts)
  }

  def densityPlot(sketch: S[_]): Option[List[(Range, Double)]] = {
    val sum = self.sum(sketch)
    countPlot(sketch).map(plot =>
      plot.map { case (range, count) => (range, count / (sum * (range.end - range.start))) }
    )
  }

}

trait SketchSyntax {

  implicit class SketchSyntaxImpl[A](sketch: Sketch[A]) {
    def update(a: A): Option[Sketch[A]] = Sketch.update(sketch, a)
    def count(from: A, to: A): Option[Double] = Sketch.count(sketch, from, to)
    def sum: Double = Sketch.sum(sketch)
//    def clear: Sketch = Sketch.clear(sketch)
    def probability(from: A, to: A): Option[Double] = Sketch.probability(sketch, from, to)
    def rearrange: Option[Sketch[A]] = Sketch.rearrange(sketch)
  }

}

object Sketch extends SketchOps[Sketch] {

  def apply[A](measure: A => Prim, structure: Structure): Sketch[A] = SimpleSketch(measure, structure)

  def empty[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): Sketch[A] =
    PeriodicSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  // mapping ops

  def primUpdate[A](sketch: Sketch[A], p: Double): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[A] => PeriodicSketch.primUpdate(sketch, p)
    case _ => SimpleSketch.primUpdate(sketch, p)
  }

//  def clear(sketch: Sketch): Sketch = sketch match {
//    case sketch: PeriodicSketch => PeriodicSketch.clear(sketch)
//  }

  def rearrange[A](sketch: Sketch[A]): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.rearrange(sketch)
    case _ => SimpleSketch.rearrange(sketch)
  }

  def modifyStructure[A](sketch: Sketch[A], f: Structure => Option[Structure]): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.modifyStructure(sketch, f)
    case _ => SimpleSketch.modifyStructure(sketch, f)
  }

}
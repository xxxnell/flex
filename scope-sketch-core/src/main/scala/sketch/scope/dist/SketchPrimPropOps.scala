package sketch.scope.dist

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.HDim
import cats.implicits._

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchPrimPropOps[S[_]<:Sketch[_]] extends SketchPrimPropLaws[S] with SketchPropOps[S]{

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

trait SketchPrimPropLaws[S[_]<:Sketch[_]] { self: SketchPrimPropOps[S] =>

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
package sketch.scope.dist

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.HDim
import cats.implicits._
import sketch.scope.dist.update.UniformCdfUpdate

/**
  * Licensed by Probe Technology, Inc.
  *
  * This Ops introduces the update function with primitive type as a parameter.
  */
trait SketchPrimPropOps[S[_]<:Sketch[_]] extends SketchPrimPropLaws[S] with SketchPropOps[S]{

  // Update ops

  /**
    * Update a primitive value <code>p</code> without rearrange process.
    * */
  def primNarrowUpdate[A](sketch: S[A], ps: List[Prim]): Option[S[A]] = modifyStructure(sketch, strs =>
    strs.traverse { case (cmap, hcounter) =>
      ps.foldLeft(Option(hcounter))((hcounterO, p) =>
        hcounterO.flatMap(hcounter => hcounter.update(cmap.apply(p), 1))
      ).map(hcounter => (cmap, hcounter))
    }
  )

  /**
    * Deep update a primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primDeepUpdate[A](sketch: S[A], ps: List[Prim]): Option[(S[A], Structure)] = for {
    utdCmap <- UniformCdfUpdate.updateCmap(sketch, ps)
    headTailStr <- sketch.structures match {
      case head :: tail => Some((head, tail))
      case _ => None
    }
    (oldStr, strs) = headTailStr
    utdHCounter <- migrate(sketch, utdCmap, oldStr._2.depth, oldStr._2.width)
    utdStrs = strs :+ (utdCmap, utdHCounter)
    utdSketch <- modifyStructure(sketch, _ => Some(utdStrs))
  } yield (utdSketch, oldStr)

  def migrate[A](sketch: S[A], cmap: Cmap, depth: Int, width: Int): Option[HCounter] = {
    cmap.ranges
      .flatMap { case (hdim, range) => primCount(sketch, range.start, range.end).map(count => (hdim, count)) }
      .foldLeft(Option(HCounter.empty(depth, width))){ case (hcounterO, (hdim, count)) =>
        hcounterO.flatMap(hcounter => hcounter.update(hdim, count))
      }
  }

  // Read ops

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
    val countsO = sketch.structures.traverse { case (cmap, hcounter) => singleCount(cmap, hcounter, pFrom, pTo) }
    countsO.map(counts => counts.sum / counts.size)
  }

  /**
    * Total number of elements be memorized.
    * */
  def sum(sketch: S[_]): Double = {
    val sums = sketch.structures.map { case (_, hcounter) => hcounter.sum }
    sums.sum / sums.size
  }

}

trait SketchPrimPropLaws[S[_]<:Sketch[_]] { self: SketchPrimPropOps[S] =>

  def narrowUpdate[A](sketch: S[A], as: List[A]): Option[S[A]] = {
    primNarrowUpdate(sketch, as.map(a => sketch.measure.asInstanceOf[A => Prim](a)))
  }

  def deepUpdate[A](sketch: S[A], as: List[A]): Option[(S[A], Structure)] = {
    primDeepUpdate(sketch, as.map(a => sketch.measure.asInstanceOf[A => Prim](a)))
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
      cmapHcounter <- sketch.structures.lastOption
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
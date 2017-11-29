package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.HDim
import cats.implicits._
import sketch.scope.pdf.update.UniformCdfUpdate
import sketch.scope.plot._
import sketch.scope.range._

/**
  * Licensed by Probe Technology, Inc.
  *
  * This Ops introduces the update function with primitive type as a parameter.
  */
trait SketchPrimPropOps[S[_]<:Sketch[_]] extends SketchPrimPropLaws[S] with SketchPropOps[S]{

  val mixingRate: Double = 1

  val window: Double = math.pow(10, -10)

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
    utdCmap <- UniformCdfUpdate.updateCmap(sketch, ps, mixingRate, window)
    headTailStr <- sketch.structures match {
      case head :: tail => Some((head, tail))
      case _ => None
    }
    (oldStr, strs) = headTailStr
    utdHCounter1 <- migrateForSketch(HCounter.empty(oldStr._2.depth, oldStr._2.width), utdCmap, sketch)
    utdHCounter2 <- migrateForPs(utdHCounter1, utdCmap, ps)
    utdStrs = strs :+ (utdCmap, utdHCounter2)
    utdSketch <- modifyStructure(sketch, _ => Some(utdStrs))
  } yield (utdSketch, oldStr)

  def migrateForSketch[A](hcounter: HCounter, cmap: Cmap, sketch: S[A]): Option[HCounter] = {
    cmap.ranges
      .flatMap { case (hdim, range) => primCount(sketch, range.start, range.end).map(count => (hdim, count)) }
      .foldLeft(Option(hcounter)){ case (hcounterO, (hdim, count)) =>
        hcounterO.flatMap(hcounter => hcounter.update(hdim, count))
      }
  }

  def migrateForPs(hcounter: HCounter, cmap: Cmap, ps: List[Prim]): Option[HCounter] = {
    ps.map(p => cmap.apply(p))
      .foldLeft(Option(hcounter))((hcounterO, hdim) => hcounterO.flatMap(hcounter => hcounter.update(hdim, 1)))
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

  def countPlot(sketch: S[_]): Option[Plot] = for {
    cmapHcounter <- sketch.structures.lastOption
    (cmap, _) = cmapHcounter
    ranges = cmap.bin.map(numRange => Range.forNumericRange(numRange))
    counts <- ranges.traverse(range => primCount(sketch, range.start, range.end))
  } yield Plot.disjoint(ranges.zip(counts))

  def densityPlot(sketch: S[_]): Option[Plot] = {
    val sum = self.sum(sketch)

    for {
      plot <- countPlot(sketch)
    } yield plot.modify { case (range, value) => value / (sum * (range.end - range.start)) }
  }

}
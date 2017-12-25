package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.HDim
import cats.implicits._
import sketch.scope.measure.Measure
import sketch.scope.pdf.update.EqualSpaceCdfUpdate
import sketch.scope.plot._
import sketch.scope.range._

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  *
  * This Ops introduces the update function with primitive type as a parameter.
  */
trait SketchPrimPropOps[S[_]<:Sketch[_]] extends SketchPrimPropLaws[S] with SketchPropOps[S]{

  val mixingRatio: Double = 1

  val window: Double = 1e-10

  // Update ops

  /**
    * Update a primitive value <code>p</code> without rearrange process.
    * */
  def primNarrowUpdate[A](sketch: S[A], ps: List[(Prim, Count)]): Option[S[A]] = modifyStructure(sketch, strs =>
    strs.traverse { case (cmap, hcounter) =>
      ps.foldLeft(Option(hcounter))((hcounterO, p) =>
        hcounterO.flatMap(hcounter => hcounter.update(cmap.apply(p._1), p._2))
      ).map(hcounter => (cmap, hcounter))
    }
  )

  /**
    * Deep update a primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primDeepUpdate[A](sketch: S[A], ps: List[(Prim, Count)]): Option[(S[A], Structure)] = for {
    utdCmap <- EqualSpaceCdfUpdate.updateCmap(sketch, ps, mixingRatio, window)
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

  def migrateForPs(hcounter: HCounter, cmap: Cmap, ps: List[(Prim, Count)]): Option[HCounter] = {
    ps.map(p => (cmap.apply(p._1), p._2))
      .foldLeft(Option(hcounter)){ case (hcounterO, (hdim, count)) =>
        hcounterO.flatMap(hcounter => hcounter.update(hdim, count))
      }
  }

  // Read ops

  def singleCount(cmap: Cmap, hcounter: HCounter, pStart: Double, pEnd: Double): Option[Double] = {
    val (startHdim, endHdim) = (cmap.apply(pStart), cmap.apply(pEnd))
    val (startRng, endRng) = (cmap.range(startHdim), cmap.range(endHdim))

    // mid count
    val midRangeO: Option[(HDim, HDim)] = if((endHdim - 1) > (startHdim + 1)) {
      Some((startHdim + 1, endHdim - 1))
    } else None
    val midCountO: Option[Double] = midRangeO.map { case (midStart, midEnd) => hcounter.count(midStart, midEnd) }
      .getOrElse(Option(0d))

    val boundaryCountO = if(startHdim == endHdim) {
      for {
        count <- hcounter.get(startHdim)
        percent = startRng.overlapPercent(RangeP(pStart, pEnd))
      } yield count * percent
    } else {
      for {
        startCount <- hcounter.get(startHdim)
        startPercent = startRng.overlapPercent(RangeP(pStart, startRng.end))
        endCount <- hcounter.get(endHdim)
        endPercent = endRng.overlapPercent(RangeP(endRng.start, pEnd))
      } yield startCount * startPercent + endCount * endPercent
    }

    for {
      boundartCount <- boundaryCountO
      midCount <- midCountO
    } yield midCount + boundartCount
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

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): Option[S[A]] = {
    primNarrowUpdate(sketch, as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) })
  }

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)]): Option[(S[A], Structure)] = {
    primDeepUpdate(sketch, as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) })
  }

  /**
    * Get the number of elements be memorized.
    * */
  def count[A](sketch: S[A], from: A, to: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primCount(sketch, measure(from), measure(to))
  }

  /***/
  def probability[A](sketch: S[A], from: A, to: A): Option[Double] = for {
    count <- count(sketch, from, to)
    sum = self.sum(sketch)
  } yield (BigDecimal(count) / BigDecimal(sum)).toDouble

  //  def pdf(sketch: S, a: Double): Option[Double] = ???

  //  def cdf(sketch: S, a: Double): Option[Double] = ???

  def countPlot(sketch: S[_]): Option[CountPlot] = for {
    cmapHcounter <- sketch.structures.lastOption
    (cmap, _) = cmapHcounter
    ranges = cmap.bin
    counts <- ranges.traverse(range => primCount(sketch, range.start, range.end))
  } yield CountPlot.disjoint(ranges.zip(counts))

  def densityPlot(sketch: S[_]): Option[DensityPlot] = {
    val sum = self.sum(sketch)

    for {
      plot <- countPlot(sketch)
      domain <- plot.domain
      flatDensity = (1d / domain.length).toDouble
    } yield DensityPlot.disjoint(
      plot.records
        .map { case (range, value) => (range, if(sum != 0) value / (sum * range.length).toDouble else flatDensity) }
        .filter { case (_, value) => !value.isNaN }
    )
  }

}

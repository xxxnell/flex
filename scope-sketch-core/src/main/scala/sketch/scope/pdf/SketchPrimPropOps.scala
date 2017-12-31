package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.HDim
import cats.implicits._
import sketch.scope.conf.SketchConf
import sketch.scope.measure.Measure
import sketch.scope.pdf.update.EqualSpaceCdfUpdate
import sketch.scope.plot._
import sketch.scope.range._

import scala.language.{higherKinds, postfixOps}
import scala.util.Try

/**
  * Licensed by Probe Technology, Inc.
  *
  * This Ops introduces the update function with primitive type as a parameter.
  */
trait SketchPrimPropOps[S[_]<:Sketch[_], C<:SketchConf]
  extends SketchPrimPropLaws[S, C]
    with SketchPropOps[S, C] { self =>

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
  def primDeepUpdate[A](sketch: S[A], ps: List[(Prim, Count)], conf: C): Option[(S[A], Structure)] = for {
    utdCmap <- EqualSpaceCdfUpdate.updateCmap(sketch, ps, conf.cmap.size, conf.mixingRatio, conf.dataKernelWindow)
    headTailStr <- sketch.structures match {
      case head :: tail => Some((head, tail))
      case _ => None
    }
    (oldStr, strs) = headTailStr
    utdHCounter1 <- migrateForSketch(HCounter.empty(oldStr._2.depth, oldStr._2.width, sum(sketch).toInt), utdCmap, sketch)
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

  def flatDensity: Double = (BigDecimal(1) / RangeP(Cmap.max, Cmap.min).length).toDouble

  def primProbability(sketch: S[_], pFrom: Prim, pTo: Prim): Option[Double] = for {
    count <- primCount(sketch, pFrom, pTo)
    sum = self.sum(sketch)
    flatProb = (flatDensity * RangeP(pFrom, pTo).length).toDouble
  } yield if(sum != 0) (BigDecimal(count) / BigDecimal(sum)).toDouble else flatProb

  def singlePdf(cmap: Cmap, counter: HCounter, p: Prim): Option[Double] = for {
    hdim <- Some(cmap.apply(p))
    records = List(counter.get(hdim - 1).map((cmap.range(hdim - 1), _)),
      counter.get(hdim).map((cmap.range(hdim), _)),
      counter.get(hdim + 1).map((cmap.range(hdim + 1), _))).flatten
    count = CountPlot.disjoint(records).interpolation(p)
    sum = counter.sum
    range = cmap.range(cmap.apply(p))
  } yield {
    if(sum != 0 && !range.isPoint) (count / (sum * range.length)).toDouble
    else if(sum == 0) flatDensity
    else if(count == 0) 0
    else Double.PositiveInfinity
  }

  def primPdf[A](sketch: S[A], p: Prim): Option[Double] = {
    val pdfsO = sketch.structures.traverse { case (cmap, counter) => singlePdf(cmap, counter, p) }

    pdfsO.map(pdfs => if(pdfs.nonEmpty && sum(sketch) != 0) pdfs.sum / pdfs.size else flatDensity)
  }

}

trait SketchPrimPropLaws[S[_]<:Sketch[_], C<:SketchConf] { self: SketchPrimPropOps[S, C] =>

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): Option[S[A]] = {
    primNarrowUpdate(sketch, as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) })
  }

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[(S[A], Structure)] = primDeepUpdate(
    sketch,
    as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) },
    conf
  )

  /**
    * Get the number of elements be memorized.
    * */
  def count[A](sketch: S[A], from: A, to: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primCount(sketch, measure(from), measure(to))
  }

  /***/
  def probability[A](sketch: S[A], from: A, to: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primProbability(sketch, measure(from), measure(to))
  }

  def countPlot(sketch: S[_]): Option[CountPlot] = for {
    cmapHcounter <- sketch.structures.lastOption
    (cmap, _) = cmapHcounter
    ranges = cmap.bin
    counts <- ranges.traverse(range => primCount(sketch, range.start, range.end))
  } yield CountPlot.disjoint(ranges.zip(counts))

  def sampling(sketch: S[_]): Option[DensityPlot] = for {
    cmapHcounter <- sketch.structures.lastOption
    (cmap, _) = cmapHcounter
    ranges = cmap.bin
    rangeProbs <- ranges.traverse(range => primProbability(sketch, range.start, range.end).map(prob => (range, prob)))
    rangeDensities = rangeProbs
      .map { case (range, prob) => (range, Try(prob / range.length).toOption) }
      .flatMap { case (range, densityO) => densityO.map(density => (range, density.toDouble)) }
  } yield DensityPlot.disjoint(rangeDensities)

  def fastPdf[A](sketch: S[A], a: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primPdf(sketch, measure(a))
  }

}

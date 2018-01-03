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
    * Update a list of primitive value <code>p</code> without rearranging process only for structures.
    * */
  def primNarrowUpdateForStr[A](sketch: S[A], ps: List[(Prim, Count)]): Option[S[A]] = modifyStructure(sketch, strs => {
    val (effStrs, refStrO) = if (strs.headOption != strs.lastOption) (strs.init, strs.lastOption) else (strs, None)
    def updatePs(cmap: Cmap, counter: HCounter, ps: List[(Prim, Count)]): Option[HCounter] =
      counter.updates(ps.map { case (p, count) => (cmap(p), count) })

    val utdEffStrsO = effStrs.traverse { case (cmap, counter) => updatePs(cmap, counter, ps).map((cmap, _)) }
    refStrO.fold(utdEffStrsO)(refStr => utdEffStrsO.map(utdEffStrs => utdEffStrs :+ refStr))
  })

  /**
    * Deep update a list of primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primDeepUpdate[A](sketch: S[A], ps: List[(Prim, Count)], conf: C): Option[(S[A], Option[Structure])] = for {
    utdCmap <- EqualSpaceCdfUpdate.updateCmap(sketch, ps, conf.cmap.size, conf.mixingRatio, conf.dataKernelWindow)
    emptyCounter = HCounter.emptyForConf(conf.counter, sumForStr(sketch).toInt)
    (utdStrs, oldStrs) = ((utdCmap, emptyCounter) :: sketch.structures).splitAt(conf.cmap.no)
    oldStrO = oldStrs.headOption
    utdSketch1 <- modifyStructure(sketch, _ => Some(utdStrs))
    utdSketch2 <- primNarrowUpdateForStr(utdSketch1, ps)
  } yield (utdSketch2, oldStrO)

  @deprecated
  def migrateForSketch[A](hcounter: HCounter, cmap: Cmap, sketch: S[A]): Option[HCounter] = {
    cmap.ranges
      .flatMap { case (hdim, range) => primCountForStr(sketch, range.start, range.end).map(count => (hdim, count)) }
      .foldLeft(Option(hcounter)){ case (hcounterO, (hdim, count)) =>
        hcounterO.flatMap(hcounter => hcounter.update(hdim, count))
      }
  }

  @deprecated
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

  def primCountForStr(sketch: S[_], pFrom: Prim, pTo: Prim): Option[Double] = {
    val countsO = sketch.structures.traverse { case (cmap, hcounter) => singleCount(cmap, hcounter, pFrom, pTo) }

    countsO.map(counts => counts.sum / counts.size)
  }

  /**
    * Total number of elements be memorized.
    * */
  def sumForStr(sketch: S[_]): Double = {
    val sums = sketch.structures.map { case (_, hcounter) => hcounter.sum }

    sums.sum / sums.size
  }

  def flatDensity: Double = (BigDecimal(1) / RangeP(Cmap.max, Cmap.min).length).toDouble

  @deprecated
  def primProbabilityForStr(sketch: S[_], pFrom: Prim, pTo: Prim): Option[Double] = for {
    count <- primCountForStr(sketch, pFrom, pTo)
    sum = self.sumForStr(sketch)
    flatProb = (flatDensity * RangeP(pFrom, pTo).length).toDouble
  } yield if(sum != 0) (BigDecimal(count) / BigDecimal(sum)).toDouble else flatProb

  def singlePdfForStr(cmap: Cmap, counter: HCounter, p: Prim): Option[Double] = for {
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

  /**
    * pdf * N (total number of counter) for single structure
    * */
  def singlePdfNForStr(cmap: Cmap, counter: HCounter, p: Prim): Option[Double] = for {
    pdf <- singlePdfForStr(cmap, counter, p)
  } yield pdf * counter.sum

  def primPdfNForStr[A](sketch: S[A], p: Prim): Option[Double] = {
    val pdfNsO = sketch.structures.traverse { case (cmap, counter) => singlePdfNForStr(cmap, counter, p) }

    pdfNsO.map(pdfNs => pdfNs.foldLeft(0d){ case (acc, pdfN) => acc + pdfN  }) // todo decay factor
  }

  def primPdfForStr[A](sketch: S[A], p: Prim): Option[Double] = for {
    pdfN <- primPdfNForStr(sketch, p)
    sum = self.sum(sketch)
  } yield pdfN / sum

}

trait SketchPrimPropLaws[S[_]<:Sketch[_], C<:SketchConf] { self: SketchPrimPropOps[S, C] =>

  def pdfNForStr[A](sketch: S[A], a: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primPdfNForStr(sketch, measure.to(a))
  }

  def countForStr[A](sketch: S[A], start: A, end: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primCountForStr(sketch, measure(start), measure(end))
  }

  def fastPdfForStr[A](sketch: S[A], a: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primPdfForStr(sketch, measure(a))
  }

  // implements the Sketch ops

  def count[A](sketch: S[A], start: A, end: A): Option[Double] = countForStr(sketch, start, end)

  def sum(sketch: S[_]): Count = sumForStr(sketch)

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[S[A]] = {
    val ps = as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) }
    primNarrowUpdateForStr(sketch, ps)
  }

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[(S[A], Option[Structure])] = {
    val ps = as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) }
    primDeepUpdate(sketch, ps, conf)
  }

  @deprecated
  def countPlot(sketch: S[_]): Option[CountPlot] = for {
    cmapHcounter <- sketch.structures.lastOption
    (cmap, _) = cmapHcounter
    ranges = cmap.bin
    counts <- ranges.traverse(range => primCountForStr(sketch, range.start, range.end))
  } yield CountPlot.disjoint(ranges.zip(counts))

  def fastPdf[A](sketch: S[A], a: A): Option[Double] = fastPdfForStr(sketch, a)

  def sample[A](dist: S[A]): (S[A], A) = ???

}

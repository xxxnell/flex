package flip.pdf

import flip.cmap.Cmap
import flip.hcounter.HCounter
import flip.hmap.HDim
import flip.conf.SketchConf
import flip.measure.Measure
import flip.pdf.update.EqualSpaceCdfUpdate
import flip.plot._
import flip.range._
import flip.range.syntax._

import scala.language.{higherKinds, postfixOps}
import cats.implicits._

/**
  * This Ops introduces the update function with primitive type as a parameter.
  */
trait SketchPrimPropOps[S[_]<:Sketch[_], C<:SketchConf]
  extends SketchPrimPropLaws[S, C]
    with SketchPropOps[S, C] { self =>

  // Update ops

  /**
    * Update a list of primitive value <code>p</code> without rearranging process only for structures.
    * */
  def primNarrowUpdateForStr[A](sketch: S[A],
                                ps: List[(Prim, Count)],
                                conf: C): Option[S[A]] = modifyStructure(sketch, strs => {
    val effNo = if(conf.cmap.no > 1) conf.cmap.no - 1 else conf.cmap.no
    val (effStrs, refStrs) = strs.splitAt(effNo)
    def updatePs(cmap: Cmap, counter: HCounter, ps: List[(Prim, Count)]): Option[HCounter] =
      counter.updates(ps.map { case (p, count) => (cmap(p), count) })
    val utdEffStrsO = effStrs.traverse { case (cmap, counter) => updatePs(cmap, counter, ps).map((cmap, _)) }
    utdEffStrsO.map(utdEffStrs => utdEffStrs ++ refStrs)
  })

  def primNarrowPlotUpdateForStr[A](sketch: S[A], pdensity: DensityPlot, conf: C): Option[S[A]] = for {
    cmap <- youngCmap(sketch)
    domain <- pdensity.domain
    (startHdim, endHdim) = (cmap.apply(domain.start), cmap.apply(domain.end))
    ps = (startHdim to endHdim).toList.map { hdim =>
      val range = cmap.range(hdim)
      val start = if(range.start > domain.start) range.start else domain.start
      val end = if(range.end < domain.end) range.end else domain.end
      (range.middle, pdensity.integral(start, end)) // todo range.middle is hacky approach
    }
    utdSketch <- primNarrowUpdateForStr(sketch, ps, conf)
  } yield utdSketch

  /**
    * Deep update a list of primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primDeepUpdate[A](sketch: S[A], ps: List[(Prim, Count)], conf: C): Option[(S[A], Option[Structure])] = for {
    utdCmap <- EqualSpaceCdfUpdate.updateCmap(sketch, ps, conf.cmap.size, conf.mixingRatio, conf.dataKernelWindow)
    seed = ((sum(sketch, conf) + ps.headOption.map(_._1).getOrElse(-1d)) * 1000).toInt
    emptyCounter = HCounter.emptyForConf(conf.counter, seed)
    (utdStrs, oldStrs) = ((utdCmap, emptyCounter) :: sketch.structures).splitAt(conf.cmap.no)
    oldStrO = oldStrs.headOption
    utdSketch1 <- modifyStructure(sketch, _ => Some(utdStrs))
    smoothPs = EqualSpaceCdfUpdate.smoothingPsForEqualSpaceCumulative(ps)
    utdSketch2 <- if(smoothPs.nonEmpty) primNarrowPlotUpdateForStr(utdSketch1, smoothPs, conf) else Some(utdSketch1)
  } yield (utdSketch2, oldStrO)

  // Read ops

  def decayRate(decayFactor: Double, i: Int): Double = math.exp(-1 * decayFactor * i)

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

  def primCountForStr(sketch: S[_], pFrom: Prim, pTo: Prim, conf: C): Option[Double] = {
    val countsO = sketch.structures.traverse { case (cmap, hcounter) => singleCount(cmap, hcounter, pFrom, pTo) }

    val decayRates = (0 until conf.cmap.no)
      .map(i => decayRate(conf.decayFactor, i))
      .take(countsO.map(_.size).getOrElse(0))
    val weightedCountSumO = countsO.map(counts => (counts zip decayRates).map { case (count, r) => count * r }.sum)
    val normalization = decayRates.sum

    weightedCountSumO.map(sum => sum / normalization)
  }

  /**
    * Total number of elements be effective memorized.
    * */
  def sumForStr(sketch: S[_], conf: C): Double = {
    val sums = sketch.structures.map { case (_, hcounter) => hcounter.sum }

    val decayRates = (0 until conf.cmap.no).map(i => decayRate(conf.decayFactor, i)).take(sums.size)
    val weightedSumSum = (sums zip decayRates).map { case (sum, r) => sum * r }.sum
    val normalization = decayRates.sum

    weightedSumSum / normalization
  }

}

trait SketchPrimPropLaws[S[_]<:Sketch[_], C<:SketchConf] { self: SketchPrimPropOps[S, C] =>

  def countForStr[A](sketch: S[A], start: A, end: A, conf: C): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primCountForStr(sketch, measure(start), measure(end), conf)
  }

  // implements the Sketch ops

  def count[A](sketch: S[A], start: A, end: A, conf: C): Option[Double] = countForStr(sketch, start, end, conf)

  def sum(sketch: S[_], conf: C): Count = sumForStr(sketch, conf)

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[S[A]] = {
    val ps = as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) }
    primNarrowUpdateForStr(sketch, ps, conf)
  }

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[(S[A], Option[Structure])] = {
    val ps = as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) }
    primDeepUpdate(sketch, ps, conf)
  }

  def sample[A](dist: S[A]): (S[A], A) = ???

}

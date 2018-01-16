package flip.pdf

import flip.time
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

import scala.collection.mutable.ListBuffer

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
                                conf: C): Option[S[A]] = modifyStructure(sketch, strs => time({
    val effNo = if(conf.cmap.no > 1) conf.cmap.no - 1 else conf.cmap.no
    val (effStrs, refStrs) = time(strs.splitAt(effNo), "strs.splitAt(effNo)", false) // 1E3
    def updatePs(cmap: Cmap, counter: HCounter, ps: List[(Prim, Count)]): Option[HCounter] =
      time(counter.updates(ps.map { case (p, count) => (cmap(p), count) }), "updatePs", false) // 4E5
    val utdEffStrsO = time(effStrs.traverse { case (cmap, counter) => updatePs(cmap, counter, ps).map(hc => (cmap, hc)) }, "utdEffStrsO", false) // 1E6
    time(utdEffStrsO.map(utdEffStrs => utdEffStrs ++ refStrs), "primNarrowUpdateForStr result", false) // 6E4
  }, "primNarrowUpdateForStr internal", false)) // 2E6

  /**
    * Deep update a list of primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primDeepUpdate[A](sketch: S[A], ps: List[(Prim, Count)], conf: C): Option[(S[A], Option[Structure])] = for {
    utdCmap <- time(EqualSpaceCdfUpdate.updateCmap(sketch, ps, conf), "updateCmap", false) // 1E7
    seed = ((sum(sketch, conf) + ps.headOption.map(_._1).getOrElse(-1d)) * 1000).toInt
    emptyCounter = HCounter.emptyForConf(conf.counter, seed)
    (utdStrs, oldStrs) = ((utdCmap, emptyCounter) :: sketch.structures).splitAt(conf.cmap.no)
    oldStrO = oldStrs.headOption
    utdSketch1 <- modifyStructure(sketch, _ => Some(utdStrs))
    smoothPs = time(EqualSpaceCdfUpdate.smoothingPsForEqualSpaceCumulative(ps), "smoothingPsForEqualSpaceCumulative", false) // 3E5
    utdSketch2 <- time(if(smoothPs.nonEmpty) primNarrowPlotUpdateForStr(utdSketch1, smoothPs, conf) else Some(utdSketch1), "primNarrowPlotUpdateForStr", false) // 3E6
  } yield (utdSketch2, oldStrO)

  def primNarrowPlotUpdateForStr[A](sketch: S[A], pdensity: DensityPlot, conf: C): Option[S[A]] = for {
    cmap <- youngCmap(sketch)
    domain <- pdensity.domain
    (startHdim, endHdim) = (cmap.apply(domain.start), cmap.apply(domain.end))
    ps = time((startHdim to endHdim).toList.map { hdim =>
      val range = cmap.range(hdim)
      val start = if(range.start > domain.start) range.start else domain.start
      val end = if(range.end < domain.end) range.end else domain.end
      (range.middle, pdensity.integral(start, end)) // todo range.middle is hacky approach
    }, "ps integration", false) // 2E6
    utdSketch <- time(primNarrowUpdateForStr(sketch, ps, conf), "primNarrowUpdateForStr", false) // 1E6
  } yield utdSketch

  // Read ops

  def decayRate(decayFactor: Double, i: Int): Double = math.exp(-1 * decayFactor * i)

  def singleCount(cmap: Cmap, hcounter: HCounter, pStart: Double, pEnd: Double): Option[Double] = {
    val (startHdim, endHdim) = time((cmap.apply(pStart), cmap.apply(pEnd)), "cmap apply", false) // 1346
    val (startRng, endRng) = time((cmap.range(startHdim), cmap.range(endHdim)), "cmap range", false) // 770

    // mid count
//    val midRangeO: Option[(HDim, HDim)] = if((endHdim - 1) > (startHdim + 1)) {
//      Some((startHdim + 1, endHdim - 1))
//    } else None
//    val midCountO: Option[Double] = midRangeO.map { case (midStart, midEnd) => hcounter.count(midStart, midEnd) }
//      .getOrElse(Option(0d))

    val midCountO: Option[Double] = time(if((endHdim - 1) > (startHdim + 1)) {
      Some(hcounter.count(startHdim + 1, endHdim - 1).getOrElse(0.0))
    } else Some(0.0), "midCountO", false) // 3062

    // boundary count
    val boundaryCountO = time(if(startHdim == endHdim) {
      time({
        for {
          count <- hcounter.get(startHdim)
          percent = startRng.overlapPercent(RangeP(pStart, pEnd))
        } yield count * percent
      }, "boundaryCountO startHdim == endHdim ", false)
    } else {
      for {
        startCount <- time(HCounter.get(hcounter, startHdim), "HCounter.get 1", false) // 1130
        startPercent = time(startRng.overlapPercent(RangeP(pStart, startRng.end)), "overlapPercent 1", false) // 298
        endCount <- time(HCounter.get(hcounter, endHdim), "HCounter.get 2", false)
        endPercent = time(endRng.overlapPercent(RangeP(endRng.start, pEnd)), "overlapPercent 2", false)
      } yield time(startCount * startPercent + endCount * endPercent, "boundaryCountO result", false)
    }, "boundaryCountO", false) // 1552

    time(for {
      midCount <- midCountO
      boundartCount <- boundaryCountO
    } yield midCount + boundartCount, "compose results", false) // 140
  }

  def primCountForStr(sketch: S[_], pFrom: Prim, pTo: Prim, conf: C): Option[Double] = {
    val countsO = time(sketch.structures.traverse { case (cmap, hcounter) => time(singleCount(cmap, hcounter, pFrom, pTo), "singleCount", false) }, "countsO functional", false) // 7e3
    val decayRates = time((0 until countsO.map(_.size).getOrElse(0))
      .map(i => decayRate(conf.decayFactor, i)), "decayRates", false) // 1365
    val weightedCountSumO = time(countsO.map(counts => (counts zip decayRates).map { case (count, r) => count * r }.sum), "weightedCountSumO", false) // 5378
    val normalization = time(decayRates.sum, "decayRates.sum", false) // 1675

    time(weightedCountSumO.map(sum => sum / normalization), "primCountForStr result", false) // 565
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

package flip.pdf

import cats.implicits._
import flip.cmap.Cmap
import flip.hcounter.HCounter
import flip.measure.Measure
import flip.pdf.update.{EqualSpaceCdfUpdate, EqualSpaceSmoothingPs, SmoothingPs}
import flip.plot._
import flip.range._
import flip.range.syntax._

import scala.language.{higherKinds, postfixOps}

/**
  * This Ops introduces the update function with primitive type as a parameter.
  */
trait SketchPrimPropOps[S[_]<:Sketch[_]]
  extends SketchPrimPropLaws[S]
    with SketchPropOps[S] { self =>

  def smoothingPs: SmoothingPs = EqualSpaceSmoothingPs

  // Update ops

  /**
    * Update a list of primitive value <code>p</code> without rearranging process only for structures.
    * */
  def primNarrowUpdateForStr[A](sketch: S[A],
                                ps: List[(Prim, Count)]): Option[S[A]] = modifyStructure(sketch, strs => {
    val cmapNo = sketch.conf.cmap.no
    val effNo = if(cmapNo > 1) cmapNo - 1 else cmapNo
    val (effStrs, refStrs) = strs.splitAt(effNo)
    def updatePs(cmap: Cmap, counter: HCounter, ps: List[(Prim, Count)]): Option[HCounter] =
      counter.updates(ps.map { case (p, count) => (cmap(p), count) })
    val utdEffStrsO = effStrs.traverse { case (cmap, counter) => updatePs(cmap, counter, ps).map(hc => (cmap, hc)) }

    utdEffStrsO.map(utdEffStrs => utdEffStrs ++ refStrs)
  })

  /**
    * Deep update a list of primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primDeepUpdate[A](sketch: S[A], ps: List[(Prim, Count)]): Option[(S[A], Option[Structure])] = for {
    utdCmap <- flip.time(EqualSpaceCdfUpdate.updateCmapForSketch(sketch, ps), "updateCmapForSketch", false) // 1.5e8 (vs 9e7, x1.5)
    seed = ((sum(sketch) + ps.headOption.map(_._1).getOrElse(-1d)) * 1000).toInt
    emptyCounter = HCounter.emptyForConf(sketch.conf.counter, seed)
    (utdStrs, oldStrs) = ((utdCmap, emptyCounter) :: sketch.structures).splitAt(sketch.conf.cmap.no)
    oldStrO = oldStrs.headOption
    utdSketch1 <- modifyStructure(sketch, _ => Some(utdStrs))
    utdSketch2 <- flip.time(if(ps.nonEmpty) {
      val smoothPs = flip.time(smoothingPs(ps, 0.5), "smoothingPsForEqualSpaceCumulative", false) // 1e7 (vs 1e6, x10)
      primNarrowPlotUpdateForStr(utdSketch1, smoothPs, ps.map(_._2).sum)
    } else Some(utdSketch1), "primNarrowPlotUpdateForStr", false) // 3e7 (3e4, x1000)
  } yield (utdSketch2, oldStrO)

  def primNarrowPlotUpdateForStr[A](sketch: S[A], psDist: Dist[Prim], sum: Double): Option[S[A]] = for {
    cmap <- youngCmap(sketch)
    ps = cmap.bin.flatMap { range =>
      // todo range.middle is hacky approach
      psDist.probability(range.start, range.end)
        .map(prob => (range.middle, prob * sum))
    }
    utdSketch <- primNarrowUpdateForStr(sketch, ps)
  } yield utdSketch

  // Read ops

  def decayRate(decayFactor: Double, i: Int): Double = math.exp(-1 * decayFactor * i)

  def singleCount(cmap: Cmap, hcounter: HCounter, pStart: Double, pEnd: Double): Option[Double] = {
    val (startHdim, endHdim) = (cmap.apply(pStart), cmap.apply(pEnd))
    val (startRng, endRng) = (cmap.range(startHdim), cmap.range(endHdim))

    // mid count
    val midCountO: Option[Double] = if((endHdim - 1) > (startHdim + 1)) {
      Some(hcounter.count(startHdim + 1, endHdim - 1).getOrElse(0.0))
    } else Some(0.0)

    // boundary count
    val boundaryCountO = if(startHdim == endHdim) {
      for {
        count <- hcounter.get(startHdim)
        percent = startRng.overlapPercent(RangeP(pStart, pEnd))
      } yield count * percent
    } else {
      for {
        startCount <- HCounter.get(hcounter, startHdim)
        startPercent = startRng.overlapPercent(RangeP(pStart, startRng.end))
        endCount <- HCounter.get(hcounter, endHdim)
        endPercent = endRng.overlapPercent(RangeP(endRng.start, pEnd))
      } yield startCount * startPercent + endCount * endPercent
    }

    for {
      midCount <- midCountO
      boundartCount <- boundaryCountO
    } yield midCount + boundartCount
  }

  def primCountForStr(sketch: S[_], pFrom: Prim, pTo: Prim): Option[Double] = {
    val countsO = sketch.structures.traverse { case (cmap, hcounter) => singleCount(cmap, hcounter, pFrom, pTo) }
    val decayRates = (0 until countsO.map(_.size).getOrElse(0))
      .map(i => decayRate(sketch.conf.decayFactor, i))
    val weightedCountSumO = countsO.map(counts => (counts zip decayRates).map { case (count, r) => count * r }.sum)
    val normalization = decayRates.sum

    weightedCountSumO.map(sum => sum / normalization)
  }

  /**
    * Total number of elements be effective memorized.
    * */
  def sumForStr(sketch: S[_]): Double = {
    val sums = sketch.structures.map { case (_, hcounter) => hcounter.sum }

    val decayRates = (0 until sketch.conf.cmap.no).map(i => decayRate(sketch.conf.decayFactor, i)).take(sums.size)
    val weightedSumSum = (sums zip decayRates).map { case (sum, r) => sum * r }.sum
    val normalization = decayRates.sum

    weightedSumSum / normalization
  }

}

trait SketchPrimPropLaws[S[_]<:Sketch[_]] { self: SketchPrimPropOps[S] =>

  def countForStr[A](sketch: S[A], start: A, end: A): Option[Double] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primCountForStr(sketch, measure(start), measure(end))
  }

  // implements the Sketch ops

  def count[A](sketch: S[A], start: A, end: A): Option[Double] = countForStr(sketch, start, end)

  def sum(sketch: S[_]): Count = sumForStr(sketch)

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): Option[S[A]] = {
    val ps = as.map { case (value, count) => (sketch.measure.asInstanceOf[Measure[A]](value), count) }
    primNarrowUpdateForStr(sketch, ps)
  }

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)]): Option[(S[A], Option[Structure])] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    val ps = as.map { case (value, count) => (measure.to(value), count) }
    primDeepUpdate(sketch, ps)
  }

  def sample[A](dist: S[A]): (S[A], A) = ???

}

package flip.pdf

import cats.data.NonEmptyList
import cats.implicits._
import flip.measure.Measure
import flip.pdf.update.{EqUpdate, EqualSpaceSmoothingPs, SmoothingPs}
import flip.pdf.syntax._
import flip.plot.PointPlot

import scala.collection.mutable
import scala.language.{higherKinds, postfixOps}

/**
  * This Ops introduces the update function with primitive type as a parameter.
  */
trait SketchPrimPropOps[S[_] <: Sketch[_]] extends SketchPrimPropLaws[S] with SketchPropOps[S] { self =>

  def smoothingPs: SmoothingPs = EqualSpaceSmoothingPs

  // Update ops

  /**
    * Update a list of primitive value <code>p</code> without rearranging process only for structures.
    * */
  def primNarrowUpdateForStrs[A](sketch: S[A], ps: List[(Prim, Count)]): S[A] =
    modifyEffStructure(sketch, hist => hist.update(ps))

  /**
    * Deep update a list of primitive value <code>p</code> instead of <code>a</code> ∈ <code>A</code>
    * */
  def primDeepUpdate[A](sketch: S[A], ps: List[(Prim, Count)]): (S[A], Option[Histogram[Double]]) = {
    val cmap1 = EqUpdate.updateCmapForSketch[A](sketch.asInstanceOf[Sketch[A]], ps)
    val (strs1, strs0) = (Histogram.forCmap(cmap1)(flip.doubleMeasure, sketch.conf) :: sketch.structures).toList
      .splitAt(sketch.conf.cmap.no)
    val sketch1 = modifyStructures(sketch, _ => NonEmptyList.fromListUnsafe(strs1))
    val sketch2 = if (ps.nonEmpty) primSmoothingNarrowUpdateForStr(sketch1, ps) else sketch1

    (sketch2, strs0.headOption)
  }

//  @Deprecated
//  def primNarrowPlotUpdateForStr[A](sketch: S[A], psDist: Dist[Prim], sum: Double): S[A] = {
//    val ps = youngCmap(sketch).bin.map { range =>
//      // todo range.middle is hacky approach
//      (range.middle, psDist.probability(range.start, range.end) * sum)
//    }
//
//    primNarrowUpdateForStrs(sketch, ps)
//  }

  def primSmoothingNarrowUpdateForStr[A](sketch: S[A], ps: List[(Prim, Count)]): S[A] = {
//    // Retrieve count cumulative plot
//    val psArr = ps.sortBy(_._1).toArray
//    var i = 0
//    val cumArr = Array.ofDim[(Double, Double)](psArr.length + 2)
//    var _cum = 0.0
//    cumArr.update(0, (Double.MinValue, 0))
//    while (i < psArr.length) {
//      val (x, count) = psArr.apply(i)
//      _cum += count
//      cumArr.update(i + 1, (x, _cum))
//      i += 1
//    }
//    cumArr.update(psArr.length + 1, (Double.MaxValue, _cum))
//    val cum = PointPlot.unsafe(cumArr)
    val cum = PointPlot.normalizedCumulative(ps)

    // NarrowUpdate counts
//    val maxCmapNo = sketch.conf.cmap.no
//    val cmapNo = self.cmapNo(sketch)
//    val effNo = if (maxCmapNo > 1) maxCmapNo - 1 else maxCmapNo
//    var j = 0
//    var _sketch = sketch
//    while (j < cmapNo && j < effNo) {
//      _sketch = modifyStructure(
//        _sketch,
//        j, {
//          hist =>
//            var k = 0
//            val bins = hist.cmap.bin.toArray
//            var cum1 = 0.0
//            var _counter = counter
//            while (k < bins.length) {
//              val cum2 = cum.interpolation(bins.apply(k).end)
//              _counter = _counter.update(k, cum2 - cum1)
//              cum1 = cum2
//              k += 1
//            }
//            (cmap, _counter)
//        }
//      )
//      j += 1
//    }
//    _sketch

    modifyEffStructure(
      sketch,
      hist =>
        hist.modifyCounter { counter =>
          val bins = hist.cmap.bin.toArray
          var i = 0
          var cump1 = 0.0
          var _counter = counter
          while (i < bins.length) {
            val cump2 = cum.interpolation(bins.apply(i).end)
            _counter = counter.update(i, cump2 - cump1)
            cump1 = cump2
            i += 1
          }
          _counter
      }
    )
  }

  // Read ops

  private var decayRateCache: mutable.Map[(Double, Int), Double] = mutable.HashMap.empty

  private val decayRateCacheLimit: Int = 100

  def decayRate(decayFactor: Double, i: Int): Double = {
    decayRateCache.getOrElse(
      (decayFactor, i), {
        val rate = math.exp(-1 * decayFactor * i)
        decayRateCache.put((decayFactor, i), rate)
        if (decayRateCache.size > decayRateCacheLimit) decayRateCache = decayRateCache.takeRight(decayRateCacheLimit)
        rate
      }
    )
  }

//  def singleCount(cmap: Cmap, hcounter: HCounter, pStart: Double, pEnd: Double): Double = {
//    val (startHdim, endHdim) = (cmap.apply(pStart), cmap.apply(pEnd))
//    val (startRng, endRng) = (cmap.range(startHdim), cmap.range(endHdim))
//
//    // mid count
//    val midCount = if ((endHdim - 1) > (startHdim + 1)) {
//      hcounter.count(startHdim + 1, endHdim - 1)
//    } else 0.0
//
//    // boundary count
//    val boundaryCount = if (startHdim == endHdim) {
//      val count = hcounter.get(startHdim)
//      val percent = startRng.overlapPercent(RangeP(pStart, pEnd))
//      count * percent
//    } else {
//      val startCount = HCounter.get(hcounter, startHdim)
//      val startPercent = startRng.overlapPercent(RangeP(pStart, startRng.end))
//      val endCount = HCounter.get(hcounter, endHdim)
//      val endPercent = endRng.overlapPercent(RangeP(endRng.start, pEnd))
//      startCount * startPercent + endCount * endPercent
//    }
//
//    midCount + boundaryCount
//  }

  def primCountForStr(sketch: S[_], pFrom: Prim, pTo: Prim): Double = {
    val counts = sketch.structures.map { hist =>
      hist.count(pFrom, pTo)
    }
    val decayRates = (0 until counts.size.toInt)
      .map(i => decayRate(sketch.conf.decayFactor, i))
    val weightedCountSum = (counts.toList zip decayRates).map { case (count, r) => count * r }.sum
    val normalization = decayRates.sum

    weightedCountSum / normalization
  }

  /**
    * Total number of elements be effective memorized.
    * */
  def sumForStr(sketch: S[_]): Double = {
    val sums = sketch.structures.map { _.sum }

    val decayRates = (0 until sketch.conf.cmap.no).map(i => decayRate(sketch.conf.decayFactor, i))
    val weightedSumSum = (sums.toList zip decayRates).map { case (sum, r) => sum * r }.sum
    val normalization = decayRates.take(sums.size.toInt).sum

    weightedSumSum / normalization
  }

}

trait SketchPrimPropLaws[S[_] <: Sketch[_]] { self: SketchPrimPropOps[S] =>

  def countForStr[A](sketch: S[A], start: A, end: A): Double = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    primCountForStr(sketch, measure(start), measure(end))
  }

  // implements the Sketch ops

  def count[A](sketch: S[A], start: A, end: A): Double = countForStr(sketch, start, end)

  def sum(sketch: S[_]): Count = sumForStr(sketch)

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): S[A] = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    val ps = as.map { case (value, count) => (measure(value), count) }

    primNarrowUpdateForStrs(sketch, ps)
  }

  def deepUpdate[A](sketch: S[A], as: List[(A, Count)]): (S[A], Option[Histogram[Double]]) = {
    val measure = sketch.measure.asInstanceOf[Measure[A]]
    val ps = as.map { case (value, count) => (measure.to(value), count) }

    primDeepUpdate(sketch, ps)
  }

}

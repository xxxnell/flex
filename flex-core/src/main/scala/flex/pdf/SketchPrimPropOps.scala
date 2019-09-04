package flex.pdf

import cats.data.NonEmptyList
import cats.implicits._
import flex.measure.Measure
import flex.pdf.syntax._
import flex.pdf.update.{ EqUpdate, EqualSpaceSmoothingPs, SmoothingPs }
import flex.plot.PointPlot

import scala.collection.mutable
import scala.language.{ higherKinds, postfixOps }

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
    val (strs1, strs0) = (Histogram.forCmap(cmap1)(flex.doubleMeasure, sketch.conf) :: sketch.structures).toList
      .splitAt(sketch.conf.cmap.no)
    val sketch1 = modifyStructures(sketch, _ => NonEmptyList.fromListUnsafe(strs1))
    val sketch2 = if (ps.nonEmpty) primSmoothingNarrowUpdateForStr(sketch1, ps) else sketch1

    (sketch2, strs0.headOption)
  }

  def primSmoothingNarrowUpdateForStr[A](sketch: S[A], ps: List[(Prim, Count)]): S[A] = {
    val cum = PointPlot.cumulative(ps)
    def updateForCum(hist: Histogram[Double], cum: PointPlot): Histogram[Double] = hist.modifyCounter { counter =>
      val bins = hist.cmap.binsArr
      var (i, cump1, _counter) = (0, 0.0, counter)
      while (i < bins.length) {
        val cump2 = cum.interpolation(bins.apply(i).end)
        _counter = _counter.update(i, cump2 - cump1)
        cump1 = cump2
        i += 1
      }
      _counter
    }

    modifyEffStructure(sketch, hist => updateForCum(hist, cum))
  }

  // Read ops

  private var decayRateCache: mutable.Map[(Double, Int), Double] = mutable.HashMap.empty

  private val decayRateCacheLimit: Int = 100

  def decayRate(decayFactor: Double, i: Int): Double =
    decayRateCache.getOrElse((decayFactor, i), {
      val rate = math.exp(-1 * decayFactor * i)
      decayRateCache.put((decayFactor, i), rate)
      if (decayRateCache.size > decayRateCacheLimit) decayRateCache = decayRateCache.takeRight(decayRateCacheLimit)
      rate
    })

  def primCountForStr(sketch: S[_], pFrom: Prim, pTo: Prim): Double = {
    val counts = sketch.structures.map { hist =>
      hist.count(pFrom, pTo)
    }
    val decayRates = (0 until counts.size.toInt).map(i => decayRate(sketch.conf.decayFactor, i))
    val weightedCountSum = counts.toList.zip(decayRates).map { case (count, r) => count * r }.sum
    val normalization = decayRates.sum

    weightedCountSum / normalization
  }

  /**
   * Total number of elements be effective memorized.
   * */
  def sumForStr(sketch: S[_]): Double = {
    val sums = sketch.structures.map { _.sum }

    val decayRates = (0 until sketch.conf.cmap.no).map(i => decayRate(sketch.conf.decayFactor, i))
    val weightedSumSum = sums.toList.zip(decayRates).map { case (sum, r) => sum * r }.sum
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

package flip.pdf

import flip.cmap.Cmap
import flip.conf.pdf.{DataBinningDistConf, SketchConf}
import flip.hcounter.HCounter
import flip.measure.Measure
import flip.plot.{DensityPlot, PointPlot}
import flip.range.RangeP
import flip.rand.IRng

import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds

trait Histogram[A] extends DataBinningDist[A] {

  def cmap: Cmap

  def counter: HCounter

}

trait HistogramOps[H[_] <: Histogram[_]] extends DataBinningDistOps[H] with HistogramLaws[H] {

  def modifyCounter[A](hist: H[A], f: HCounter => HCounter): H[A]

}

trait HistogramLaws[H[_] <: Histogram[_]] { self: HistogramOps[H] =>

  def update[A](hist: H[A], as: List[(A, Count)]): H[A] =
    modifyCounter(
      hist,
      counter => {
        val measure = hist.measure.asInstanceOf[Measure[A]]
        val zs = as.map { case (a, count) => (hist.cmap.apply(measure.to(a)), count) }
        counter.updates(zs)
      }
    )

  def scanUpdate[A](hist: H[A], as: List[(A, Count)]): H[A] =
    modifyCounter(
      hist,
      counter => {
        val measure = hist.measure.asInstanceOf[Measure[A]]
        val ps = as.map { case (a, count) => (measure.to(a), count) }.toArray
        val bins = hist.cmap.binsArr

        var i = 0
        var j = 0
        var _counter = counter
        while (i < bins.length && j < ps.length) {
          val range = bins.apply(i)
          while (j < ps.length && range.contains(ps.apply(j)._1)) {
            _counter = _counter.update(i, ps.apply(j)._2)
            j += 1
          }
          i += 1
        }
        _counter
      }
    )

  def count[A](hist: H[A], start: A, end: A): Count = {
    val measure = hist.measure.asInstanceOf[Measure[A]]
    primCount(hist, measure.to(start), measure.to(end))
  }

  def primCount[A](hist: H[A], pStart: Double, pEnd: Double): Double = {
    val cmap = hist.cmap
    val counter = hist.counter
    val (startHdim, endHdim) = (cmap.apply(pStart), cmap.apply(pEnd))
    val (startRng, endRng) = (cmap.range(startHdim), cmap.range(endHdim))

    // mid count
    val midCount = if ((endHdim - 1) > (startHdim + 1)) {
      counter.count(startHdim + 1, endHdim - 1)
    } else 0.0

    // boundary count
    val boundaryCount = if (startHdim == endHdim) {
      val count = counter.get(startHdim)
      val percent = startRng.overlapPercent(RangeP(pStart, pEnd))
      count * percent
    } else {
      val startCount = HCounter.get(counter, startHdim)
      val startPercent = startRng.overlapPercent(RangeP(pStart, startRng.end))
      val endCount = HCounter.get(counter, endHdim)
      val endPercent = endRng.overlapPercent(RangeP(endRng.start, pEnd))
      startCount * startPercent + endCount * endPercent
    }

    midCount + boundaryCount
  }

  def sum(hist: H[_]): Count = hist.counter.sum

  def probability[A](dist: Histogram[A], start: A, end: A): Double = ???

  def sampling[A](dist: Histogram[A]): PointPlot = pointSampling(dist)

  def rangeSampling[A](hist: Histogram[A]): DensityPlot = ???

  def pointSampling[A](hist: Histogram[A]): PointPlot = {
    val bins = hist.cmap.binsArr
    var i = 1
    val records = new ArrayBuffer[(Double, Double)]
    while (i < bins.length - 1) {
      val count = hist.counter.get(i)
      val range = bins.apply(i)
      if (!range.isPoint) records.append((range.cutoffMiddle, count / range.cutoffLength))
      i += 1
    }
    PointPlot.unsafe(records.toArray)
  }

}

object Histogram extends HistogramOps[Histogram] {

  private case class HistogramImpl[A](measure: Measure[A],
                                      rng: IRng,
                                      conf: DataBinningDistConf,
                                      cmap: Cmap,
                                      counter: HCounter)
      extends Histogram[A]

  def bare[A](measure: Measure[A], rng: IRng, conf: DataBinningDistConf, cmap: Cmap, counter: HCounter): Histogram[A] =
    HistogramImpl(measure, rng, conf, cmap, counter)

  def empty[A](implicit measure: Measure[A], conf: DataBinningDistConf): Histogram[A] = {
    bare(measure, IRng(-1), conf, Cmap(conf.cmap), counter(conf, -1))
  }

  def forCmap[A](cmap: Cmap)(implicit measure: Measure[A], conf: DataBinningDistConf): Histogram[A] =
    bare(measure, IRng(cmap.hashCode()), conf, cmap, counter(conf, cmap.hashCode))

  def counter(conf: DataBinningDistConf, seed: Int): HCounter =
    if (conf.cmap.size > conf.counter.size) HCounter(conf.counter, -1)
    else HCounter.emptyUncompressed(conf.cmap.size)

  def modifyRng[A](hist: Histogram[A], f: IRng => IRng): flip.pdf.Histogram[A] =
    bare(hist.measure, f(hist.rng), hist.conf, hist.cmap, hist.counter)

  def modifyCounter[A](hist: Histogram[A], f: HCounter => HCounter): Histogram[A] =
    bare(hist.measure, hist.rng, hist.conf, hist.cmap, f(hist.counter))

}

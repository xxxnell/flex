package flip.pdf.monad

import cats.data.NonEmptyList
import flip.conf.pdf.{DistConf, SamplingDistConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.{DeltaDist, Dist, PlottedDist, UniformDist}
import flip.plot.PointPlot

import scala.collection.mutable.ArrayBuffer

object PointToPointBind { self =>

  val cutoff = 1E300 // TODO CUTOFF is hacky approach

  def bind[A, B](dist: Dist[A], f: A => Dist[B], measureB: Measure[B], conf: DistConf): Dist[B] = {
    val wds = weightDists(dist, f, measureB, conf)
    val wcs = NonEmptyList.fromListUnsafe(wds.map { case (w, d) => (w, d.sampling.normalizedCumulative) })
    val cum = PointPlot.add(wcs)
    val pdf = PointPlot.unsafe(cum.avgChangeRate.records.filter { case (x, _) => math.abs(x) < cutoff })

    PlottedDist.pointPlot(pdf)(measureB, SamplingDistConf.forDistConf(conf))
  }

  def weightDists[A, B](dist: Dist[A],
                        f: A => Dist[B],
                        measureB: Measure[B],
                        conf: DistConf): List[(Double, Dist[B])] = {
    val cum = dist.sampling.normalizedCumulative.records
    val array = new ArrayBuffer[(Double, Dist[B])]
    var i = 1
    while (i < cum.length) {
      val (x0, y0) = cum.apply(i - 1)
      val (x1, y1) = cum.apply(i)
      val _dist = f(dist.measure.from(x0)) match {
        case delta: DeltaDist[B] =>
          UniformDist(delta.pole, x1 - x0)(measureB, SmoothDistConf.forDistConf(conf))
        case dist0 => dist0
      }
      array.append((y1 - y0, _dist))
      i += 1
    }

    array.toList
  }

}

package flex.pdf.monad

import cats.data.NonEmptyList
import flex.conf.pdf.{DistConf, SamplingDistConf, SmoothDistConf}
import flex.measure.Measure
import flex.pdf.{DeltaDist, Dist, PlottedDist, UniformDist}
import flex.plot.PointPlot

import scala.collection.mutable.ArrayBuffer

object PointToPointBind { self =>

  def bind[A, B](dist: Dist[A], f: A => Dist[B], measureB: Measure[B], conf: DistConf): Dist[B] = {
    val wcs = weightCdfs(dist, f, measureB, conf)
    val cdf = PointPlot.add(NonEmptyList.fromListUnsafe(wcs))

    PlottedDist.forCdfSampling(cdf)(measureB, SamplingDistConf.forDistConf(conf))
  }

  def weightCdfs[A, B](dist: Dist[A],
                       f: A => Dist[B],
                       measureB: Measure[B],
                       conf: DistConf): List[(Double, PointPlot)] = {
    val cum1 = dist.cdfSampling.records
    val array = new ArrayBuffer[(Double, PointPlot)]
    var i = 1
    while (i < cum1.length) {
      val (x0, y0) = cum1.apply(i - 1)
      val (x1, y1) = cum1.apply(i)
      val cum2 = f(dist.measure.from(x0)) match {
        case delta: DeltaDist[A] =>
          UniformDist.apply(delta.pole, x1 - x0)(measureB, SmoothDistConf.forDistConf(conf)).cdfSampling
        case _dist => _dist.cdfSampling
      }
      array.append((y1 - y0, cum2))
      i += 1
    }

    array.toList
  }

}

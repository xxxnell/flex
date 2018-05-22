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
    val wcs = weightCum(dist, f, measureB, conf)
    val cum = PointPlot.add(NonEmptyList.fromListUnsafe(wcs))
    val pdf = PointPlot.unsafe(cum.avgChangeRate.records.filter { case (x, _) => math.abs(x) < cutoff })

    PlottedDist.pointPlot(pdf)(measureB, SamplingDistConf.forDistConf(conf))
  }

  def weightCum[A, B](dist: Dist[A],
                      f: A => Dist[B],
                      measureB: Measure[B],
                      conf: DistConf): List[(Double, PointPlot)] = {
    val measure = dist.measure
    val ε = dist.conf.delta
    val cum1 = dist.sampling.normalizedCumulative.records
    val array = new ArrayBuffer[(Double, PointPlot)]
    var i = 1
    while (i < cum1.length) {
      val (x0, y0) = cum1.apply(i - 1)
      val (x1, y1) = cum1.apply(i)
      val cum2 = f(dist.measure.from(x0)) match {
        case delta: DeltaDist[A] =>
          val pole = measure.to(delta.pole)
          PointPlot.unsafe(Array((pole, 0), (pole + pole * ε, 1)))
        case _dist => _dist.sampling.normalizedCumulative
      }
      array.append((y1 - y0, cum2))
      i += 1
    }

    array.toList
  }

}

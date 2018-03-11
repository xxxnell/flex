package flip.pdf.monad

import flip.conf.{DistConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.arithmetic.Sum
import flip.pdf.{DeltaDist, Dist, SamplingDist, UniformDist}

object PointToPointBind extends DistBind[SamplingDist, Dist, Dist, DistConf] {

  def bind[A, B](dist: SamplingDist[A], f: A => Dist[B], measureB: Measure[B], conf: DistConf): Dist[B] = {
    val sampling = dist.sampling
    val weightDists = sampling.records.map {
      case (range, value) =>
        val weight = range.roughLength * value
        val pointDist = f(dist.measure.from(range.middle)) match {
          case delta: DeltaDist[B] => UniformDist(delta.pole, range.roughLength)(measureB, SmoothDistConf.default)
          case d => d
        }

        (weight, pointDist)
    }

    Sum.weightedSum(weightDists, measureB)
  }

}

package flip.pdf.monad

import flip.conf.{DistConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.arithmetic.Sum
import flip.pdf.{DeltaDist, Dist, NumericDist, SamplingDist, Sketch, UniformDist}

object PointToPointBind { self =>

  def bind[A, B](dist: Dist[A], f: A => Dist[B], measureB: Measure[B], conf: DistConf): Dist[B] = {
    Sum.weightedSum(self.weightDists(dist, f, measureB, conf), measureB, conf)
  }

  def weightDists[A, B](dist: Dist[A],
                        f: A => Dist[B],
                        measureB: Measure[B],
                        conf: DistConf): List[(Double, Dist[B])] = {
    dist.sampling.records.map {
      case (range, value) =>
        val weight = range.roughLength * value
        val pointDist = f(dist.measure.from(range.middle)) match {
          case delta: DeltaDist[B] =>
            UniformDist(delta.pole, range.roughLength)(measureB, SmoothDistConf.forDistConf(conf))
          case d => d
        }

        (weight, pointDist)
    }
  }

  def samplingPoints[A](dist: Dist[A], samplingNo: Int): List[A] = dist match {
    case sketch: Sketch[A] =>
      sketch.samplingPoints.flatMap(ps => ps.start :: ps.end :: Nil).distinct
    case delta: DeltaDist[A] =>
      val measure = delta.measure
      val poleP = measure.to(delta.pole)
      val width = delta.conf.delta
      measure.from(poleP - width) :: measure.from(poleP + width) :: Nil
    case numeric: NumericDist[A] =>
      val unit = 1 / (samplingNo.toDouble + 1)
      (0 to samplingNo).toList.map(i => numeric.icdf(i * unit))
  }

}

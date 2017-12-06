package sketch.scope.pdf

import sketch.scope.measure.Measure
import sketch.scope.plot.DensityPlot
import sketch.scope.range.RangeP

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDist[A] extends Dist[A]

trait SmoothDistPropOps[D[_]<:SmoothDist[_]] extends DistPropOps[D] {

  def toSampleDist[A](dist: D[A], domains: List[RangeP]): SampledDist[A] = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val densityPlot = DensityPlot.disjoint(domains.flatMap(range =>
      probability(dist, measure.from(range.start), measure.from(range.end)).map(prob => (range, prob))
    ))

    PlottedDist(measure, densityPlot)
  }

}

object SmoothDist extends SmoothDistPropOps[SmoothDist] {

  def probability[A](dist: SmoothDist[A], start: A, end: A): Option[Prim] = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.probability(predefined, start, end)
    case delta: DeltaDist[A] => DeltaDist.probability(delta, start, end)
    case normal: NormalDist[A] => NormalDist.probability(normal, start, end)
  }

  def sample[A](dist: SmoothDist[A]): (A, SmoothDist[A]) = dist match {
    case predefined: PredefinedDist[A] => PredefinedDist.sample(predefined)
    case delta: DeltaDist[A] => DeltaDist.sample(delta)
    case normal: NormalDist[A] => NormalDist.sample(normal)
  }

}
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

  def toSampleDist[A](dist: D[A], domains: List[RangeP]): SampleDist[A] = {
    val measure = dist.measure.asInstanceOf[Measure[A]]
    val densityPlot = DensityPlot.disjoint(domains.flatMap(range =>
      probability(dist, measure.from(range.start), measure.from(range.end)).map(prob => (range, prob))
    ))

    PlottedDist(measure, densityPlot)
  }

}

object SmoothDist extends SmoothDistPropOps[SmoothDist] {

  def probability[A](dist: SmoothDist[A], from: A, to: A): Option[Prim] = dist match {
    case dist: DeltaDist[A] => DeltaDist.probability(dist, from, to)
  }

}
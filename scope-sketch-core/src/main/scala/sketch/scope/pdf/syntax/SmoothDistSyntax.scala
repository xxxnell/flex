package sketch.scope.pdf.syntax

import sketch.scope.pdf.{Dist, Prim, SamplingDist, SmoothDist}
import sketch.scope.range.{RangeM, RangeP}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {
    def sampling(domains: List[RangeM[A]]): Option[SamplingDist[A]] = SmoothDist.toSamplingDist(dist, domains)
    def sampling(samplingDist: SamplingDist[A]): Option[SamplingDist[A]] = for {
      densityPlot <- samplingDist.densityPlot
      domainsP = densityPlot.records.map(_._1)
      domainsM = domainsP.map(rangeP => rangeP.modifyMeasure(samplingDist.measure))
      dist <- SmoothDist.toSamplingDist(dist, domainsM)
    } yield dist
    def pdf(a: A): Option[Double] = SmoothDist.pdf(dist, a)
    def sample: (SmoothDist[A], A) = SmoothDist.sample(dist)
    def samples(n: Int): (SmoothDist[A], List[A]) = SmoothDist.samples(dist, n)
  }

}
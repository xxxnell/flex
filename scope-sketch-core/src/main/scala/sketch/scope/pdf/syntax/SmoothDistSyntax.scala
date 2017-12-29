package sketch.scope.pdf.syntax

import sketch.scope.pdf.{Dist, Prim, SampledDist, SmoothDist}
import sketch.scope.range.{RangeM, RangeP}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {
    def toSampleDist(domains: List[RangeM[A]]): Option[SampledDist[A]] = SmoothDist.toSampleDist(dist, domains)
    def pdf(a: A): Option[Double] = SmoothDist.pdf(dist, a)
    def sample: (SmoothDist[A], A) = SmoothDist.sample(dist)
    def samples(n: Int): (SmoothDist[A], List[A]) = SmoothDist.samples(dist, n)
  }

}
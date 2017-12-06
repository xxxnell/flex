package sketch.scope.pdf.syntax

import sketch.scope.pdf.{Dist, SampledDist, SmoothDist}
import sketch.scope.range.RangeP

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {
    def toSampleDist(domains: List[RangeP]): SampledDist[A] = SmoothDist.toSampleDist(dist, domains)
    def sample: (SmoothDist[A], A) = SmoothDist.sample(dist)
    def samples(n: Int): (SmoothDist[A], List[A]) = SmoothDist.samples(dist, n)
  }

}
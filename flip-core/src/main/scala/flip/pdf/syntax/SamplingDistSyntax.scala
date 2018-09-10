package flip.pdf.syntax

import flip.pdf.SamplingDist

import scala.language.higherKinds

trait SamplingDistSyntax extends SamplingDistPropSyntax

trait SamplingDistPropSyntax {

  implicit class SamplingDistPropSyntaxImpl[A](dist: SamplingDist[A]) {
    def sample: (SamplingDist[A], A) = SamplingDist.sample(dist)
    def pdf(a: A): Double = SamplingDist.pdf(dist, a)
    def samples(n: Int): (SamplingDist[A], List[A]) = SamplingDist.samples(dist, n)
  }

}

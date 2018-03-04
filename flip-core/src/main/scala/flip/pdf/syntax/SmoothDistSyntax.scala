package flip.pdf.syntax

import flip.conf.SamplingDistConf
import flip.pdf.{PlottedDist, SamplingDist, SmoothDist}
import flip.range.RangeM
import flip.range.syntax.RangeP

trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {
    def pdf(a: A): Double = SmoothDist.pdf(dist, a)
    def sample: (SmoothDist[A], A) = SmoothDist.sample(dist)
    def samples(n: Int): (SmoothDist[A], List[A]) = SmoothDist.samples(dist, n)
  }

  implicit class PlottedDistPropSyntaxImpl[A](dist: PlottedDist[A]) {
    def filter(f: RangeP => Boolean): PlottedDist[A] = PlottedDist.filter(dist, f)
  }

}

package flip.pdf.syntax

import flip.conf.SamplingDistConf
import flip.pdf.{PlottedDist, SamplingDist, SmoothDist}
import flip.range.RangeM
import flip.range.syntax.RangeP

trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {
    def pdf(a: A): Option[Double] = SmoothDist.pdf(dist, a)
    def sample: (SmoothDist[A], A) = SmoothDist.sample(dist)
    def samples(n: Int): (SmoothDist[A], List[A]) = SmoothDist.samples(dist, n)
    def sampling(domains: List[RangeM[A]]): Option[PlottedDist[A]] =
      SmoothDist.samplingDist(dist, domains)
    def sampling(smplDist: SamplingDist[A], smplConf: SamplingDistConf): Option[PlottedDist[A]] =
      SmoothDist.samplingDistForSamplingDist(dist, smplDist, smplConf)
    def uniformSampling(start: A, end: A, size: Int): Option[PlottedDist[A]] =
      SmoothDist.uniformSampling(dist, start, end, size)
  }

  implicit class PlottedDistPropSyntaxImpl[A](dist: PlottedDist[A]) {
    def filter(f: RangeP => Boolean): PlottedDist[A] = PlottedDist.filter(dist, f)
  }

}
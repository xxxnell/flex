package flip.pdf.syntax

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.pdf.monad.SamplingDistMonad
import flip.pdf.{Dist, SamplingDist}
import flip.plot.DensityPlot

trait SamplingDistSyntax extends SamplingDistPropSyntax with SamplingDistMonadSyntax

trait SamplingDistPropSyntax {

  implicit class SamplingDistPropSyntaxImpl[A](dist: SamplingDist[A]) {
    def sample: (SamplingDist[A], A) = SamplingDist.sample(dist)
    def pdf(a: A): Double = SamplingDist.pdf(dist, a)
    def samples(n: Int): (SamplingDist[A], List[A]) = SamplingDist.samples(dist, n)
    def sampling: DensityPlot = SamplingDist.sampling(dist)
    def densityPlot: DensityPlot = SamplingDist.sampling(dist)
    def pdfPlot: DensityPlot = SamplingDist.sampling(dist)
  }

}

trait SamplingDistMonadSyntax {

  lazy val distMonad: SamplingDistMonad[SamplingDist, Dist, SamplingDist] = SamplingDistMonad()

  implicit class SamplingDistMonadSyntaxImpl[A](dist: SamplingDist[A]) {
    def map[B](f: A => B)(implicit measureB: Measure[B]): SamplingDist[B] =
      distMonad.map(dist, f, measureB)
    def flatMap[B](f: A => Dist[B])(implicit measureB: Measure[B]): SamplingDist[B] =
      distMonad.bind(dist, f, measureB)
  }

}

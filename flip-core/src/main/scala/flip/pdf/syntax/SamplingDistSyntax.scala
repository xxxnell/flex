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

  lazy val samplingDistMonad: SamplingDistMonad[SamplingDist, Dist, SamplingDist, SamplingDistConf] = ???

  implicit class SamplingDistMonadSyntaxImpl[A](dist: SamplingDist[A]) {
    def map[B](f: A => B)(implicit measureB: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
      samplingDistMonad.map(dist, f, measureB, conf)
    def flatMap[B](f: A => Dist[B])(implicit measureB: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
      samplingDistMonad.bind(dist, f, measureB, conf)
  }

}

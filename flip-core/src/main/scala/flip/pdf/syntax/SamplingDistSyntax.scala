package flip.pdf.syntax

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.pdf.monad.SamplingDistMonad
import flip.pdf.{Dist, SamplingDist}
import flip.plot.DensityPlot

trait SamplingDistSyntax
  extends SamplingDistPropSyntax
    with SamplingDistMonadSyntax

trait SamplingDistPropSyntax {

  implicit class SamplingDistPropSyntaxImpl[A](dist: SamplingDist[A]) {
    def sample: (SamplingDist[A], A) = SamplingDist.sample(dist)
    def pdf(a: A)(implicit conf: SamplingDistConf): Option[Double] = SamplingDist.interpolationPdf(dist, a, conf)
    def samples(n: Int): (SamplingDist[A], List[A]) = SamplingDist.samples(dist, n)
    def sampling(implicit conf: SamplingDistConf): Option[DensityPlot] = SamplingDist.sampling(dist, conf)
    def densityPlot(implicit conf: SamplingDistConf): Option[DensityPlot] = SamplingDist.sampling(dist, conf)
  }

}

trait SamplingDistMonadSyntax {

  lazy val distMonad: SamplingDistMonad[SamplingDist, Dist, SamplingDist, SamplingDistConf] = SamplingDistMonad()

  implicit class SamplingDistMonadSyntaxImpl[A](dist: SamplingDist[A]) {
    def map[B](f: A => B)(implicit measureB: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
      distMonad.map(dist, f, measureB, conf)
    def flatMap[B](f: A => Dist[B])(implicit measureB: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
      distMonad.bind(dist, f, measureB, conf)
  }

}

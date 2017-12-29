package sketch.scope.pdf.syntax

import sketch.scope.measure.Measure
import sketch.scope.pdf.monad.SamplingDistMonad
import sketch.scope.pdf.{Dist, SamplingDist}
import sketch.scope.plot.DensityPlot

/**
  * Licensed by Probe Technology, Inc.
  */
trait SamplingDistSyntax
  extends SamplingDistPropSyntax
    with SamplingDistMonadSyntax

trait SamplingDistPropSyntax {

  implicit class SamplingDistPropSyntaxImpl[A](dist: SamplingDist[A]) {
    def sample: (SamplingDist[A], A) = SamplingDist.sample(dist)
    def pdf(a: A): Option[Double] = SamplingDist.pdf(dist, a)
    def samples(n: Int): (SamplingDist[A], List[A]) = SamplingDist.samples(dist, n)
    def densityPlot: Option[DensityPlot] = SamplingDist.densityPlot(dist)
  }

}

trait SamplingDistMonadSyntax {

  lazy val distMonad: SamplingDistMonad[SamplingDist, Dist, SamplingDist] = SamplingDistMonad()

  implicit class SamplingDistMonadSyntaxImpl[A](dist: SamplingDist[A]) {
    def map[B](f: A => B)(implicit measureB: Measure[B]): SamplingDist[B] = distMonad.map(dist, f, measureB)
    def flatMap[B](f: A => Dist[B])(implicit measureB: Measure[B]): SamplingDist[B] = distMonad.bind(dist, f, measureB)
  }

}

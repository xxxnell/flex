package sketch.scope.pdf.syntax

import sketch.scope.measure.Measure
import sketch.scope.pdf.monad.SampleDistMonad
import sketch.scope.pdf.{Dist, Range, SampledDist}
import sketch.scope.plot.{DensityPlot, Plot}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDistSyntax extends SampleDistPropSyntax with SampleDistMonadSyntax

trait SampleDistPropSyntax {

  implicit class SampleDistPropSyntaxImpl[A](dist: SampledDist[A]) {
    def densityPlot: Option[DensityPlot] = SampledDist.densityPlot(dist)
  }

}

trait SampleDistMonadSyntax {

  lazy val distMonad: SampleDistMonad[SampledDist, Dist, SampledDist] = SampleDistMonad()

  implicit class SampleDistMonadSyntaxImpl[A](dist: SampledDist[A]) {
    def map[B](f: A => B)(implicit measureB: Measure[B]): SampledDist[B] = distMonad.map(dist, f, measureB)
    def flatMap[B](f: A => Dist[B])(implicit measureB: Measure[B]): SampledDist[B] = distMonad.bind(dist, f, measureB)
  }

}

package sketch.scope.pdf.syntax

import sketch.scope.measure.Measure
import sketch.scope.pdf.monad.SampleDistMonad
import sketch.scope.pdf.{Dist, Range, SampleDist}
import sketch.scope.plot.{DensityPlot, Plot}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDistSyntax extends SampleDistPropSyntax with SampleDistMonadSyntax

trait SampleDistPropSyntax {

  implicit class SampleDistPropSyntaxImpl[A](dist: SampleDist[A]) {
    def densityPlot: Option[DensityPlot] = SampleDist.densityPlot(dist)
  }

}

trait SampleDistMonadSyntax {

  lazy val distMonad: SampleDistMonad[SampleDist, Dist, SampleDist] = SampleDistMonad()

  implicit class SampleDistMonadSyntaxImpl[A](dist: SampleDist[A]) {
    def map[B](f: A => B)(implicit measureB: Measure[B]): SampleDist[B] = distMonad.map(dist, f, measureB)
    def flatMap[B](f: A => Dist[B])(implicit measureB: Measure[B]): SampleDist[B] = distMonad.bind(dist, f, measureB)
  }

}

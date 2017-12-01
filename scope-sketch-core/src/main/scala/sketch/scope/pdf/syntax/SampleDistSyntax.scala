package sketch.scope.pdf.syntax

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

  implicit class SampleDistMonadSyntaxImpl[A](dist: SampleDist[A]) {
    def map[B](f: A => B): SampleDist[B] = SampleDistMonad().map(dist, f)
    def flatMap[B](f: A => Dist[B]): SampleDist[B] = SampleDistMonad().bind(dist, f)
  }

}

package sketch.scope.sketch.syntax

import sketch.scope.sketch.algebra.SampleDistMonad
import sketch.scope.sketch.{Dist, Range, SampleDist}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDistSyntax extends SampleDistPropSyntax with SampleDistMonadSyntax

trait SampleDistPropSyntax {

  implicit class SampleDistPropSyntaxImpl[A](dist: SampleDist[A]) {
    def densityPlot: Option[List[(Range, Double)]] = SampleDist.densityPlot(dist)
  }

}

trait SampleDistMonadSyntax {

  implicit class SampleDistMonadSyntaxImpl[A](dist: SampleDist[A]) {
    def map[B](f: A => B): SampleDist[B] = SampleDistMonad().map(dist, f)
    def flatMap[B](f: A => Dist[B]): SampleDist[B] = SampleDistMonad().bind(dist, f)
  }

}

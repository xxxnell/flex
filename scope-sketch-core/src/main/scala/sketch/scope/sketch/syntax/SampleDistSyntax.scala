package sketch.scope.sketch.syntax

import sketch.scope.sketch.{Range, SampleDist}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDistSyntax extends SampleDistPropSyntax

trait SampleDistPropSyntax {

  implicit class SampleDistPropSyntaxImpl[A](dist: SampleDist[A]) {
    def densityPlot: Option[List[(Range, Double)]] = ???
  }

}

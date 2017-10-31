package sketch.scope.sketch.syntax

import sketch.scope.sketch.SmoothDist

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {}

}
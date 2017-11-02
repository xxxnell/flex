package sketch.scope.dist.syntax

import sketch.scope.dist.SmoothDist

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDistSyntax extends SmoothDistPropSyntax

trait SmoothDistPropSyntax {

  implicit class SmoothDistPropSyntaxImpl[A](dist: SmoothDist[A]) {}

}
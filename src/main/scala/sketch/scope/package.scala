package sketch

import cats.data.Kleisli
import sketch.scope.sketch.SketchSyntax

/**
  * Licensed by Probe Technology, Inc.
  */
package object scope extends SketchSyntax {

  type Result[A] = Throwable Either A

  type Mon[A, B] = Kleisli[Some, A, B]

  type Epi[A, B] = Kleisli[Option, A, B]

}

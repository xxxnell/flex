package sketch

import cats.data.Kleisli
import sketch.scope.pdf.syntax.SketchSyntax

import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  */
package object scope {

  type Result[A] = Throwable Either A

  type Mon[A, B] = Kleisli[Some, A, B]

  type Epi[A, B] = Kleisli[Option, A, B]

}

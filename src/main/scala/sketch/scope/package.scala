package sketch

import sketch.scope.sketch.SketchSyntax

/**
  * Licensed by Probe Technology, Inc.
  */
package object scope extends SketchSyntax {

  type Result[A] = Throwable Either A

}

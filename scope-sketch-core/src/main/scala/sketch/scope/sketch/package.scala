package sketch.scope

import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  */
package object sketch {

  type Prim = Double

  type Range = NumericRange[Prim]

  type Ranges = List[Range]

  implicit def autocast[A](sketch: Sketch[A]): SimpleSketch[A] = SimpleSketch.sketch2SimpleSketch(sketch)

}

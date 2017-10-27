package sketch.scope

import _root_.sketch.scope.cmap.Cmap
import _root_.sketch.scope.hcounter.HCounter
import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  */
package object sketch {

  type Prim = Double

  type Range = NumericRange[Prim]

  type Ranges = List[Range]

  type Structure = List[(Cmap, HCounter)]

  implicit def autocast[A](sketch: Sketch[A]): SimpleSketch[A] = SimpleSketch.sketch2SimpleSketch(sketch)

}

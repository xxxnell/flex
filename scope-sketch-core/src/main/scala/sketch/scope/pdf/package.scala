package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

/**
  * Licensed by Probe Technology, Inc.
  */
object `package` {

  type Prim = Double

  type Count = Double

  type Range = NumericRange[Prim]

  type Ranges = List[Range]

  type Structure = (Cmap, HCounter)

  type Structures = List[Structure]

  implicit def autocast[A](sketch: Sketch[A]): SimpleSketch[A] = SimpleSketch.sketch2SimpleSketch(sketch)

}

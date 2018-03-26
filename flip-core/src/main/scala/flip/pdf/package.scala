package flip

import cats.data.NonEmptyList
import flip.cmap.Cmap
import flip.hcounter.HCounter

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

package object pdf {

  type Prim = Double

  type Count = Double

  type Range = NumericRange[Prim]

  type Ranges = List[Range]

  type Structure = (Cmap, HCounter)

  type Structures = NonEmptyList[Structure]

  implicit def autocast[A](sketch: Sketch[A]): SimpleSketch[A] = SimpleSketch.sketch2SimpleSketch(sketch)

}

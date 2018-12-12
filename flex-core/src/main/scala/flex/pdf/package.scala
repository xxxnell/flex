package flex

import cats.data.NonEmptyList
import flex.cmap.Cmap
import flex.hcounter.HCounter

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

package object pdf {

  type Prim = Double

  type Count = Double

  type Range = NumericRange[Prim]

  type Ranges = List[Range]

//  type Structure = Histogram[]

  type Structures = NonEmptyList[Histogram[Double]]

  implicit def autocast[A](sketch: Sketch[A]): SimpleSketch[A] = SimpleSketch.sketch2SimpleSketch(sketch)

}

package flex

import cats.data.NonEmptyList
import flex.cmap.Cmap
import flex.hcounter.HCounter
import org.nd4j.linalg.api.ndarray.INDArray

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

package object pdf {

  type Prim = Double

  type Count = Double

  type Range = NumericRange[Prim]

  type Ranges = List[Range]

//  type Structure = Histogram[]

  type Structures = NonEmptyList[Histogram[Double]]

  type Vec = INDArray

  type SumVec = List[Vec]

  implicit def autocast[A](sketch: Sketch[A]): SimpleSketch[A] = SimpleSketch.sketch2SimpleSketch(sketch)

}

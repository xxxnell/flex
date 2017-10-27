package sketch.scope

import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  */
package object sketch {

  type Range = NumericRange[Double]

  type Ranges = List[Range]

  type Prim = Double

}

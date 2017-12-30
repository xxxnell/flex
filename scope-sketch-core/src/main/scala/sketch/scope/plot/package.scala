package sketch.scope.plot

import sketch.scope.range.syntax._

/**
  * Licensed by Probe Technology, Inc.
  */
object `package` {

  type Record = (RangeP, Double)

  object syntax
    extends PlotSyntax
      with DensityPlotSyntax
      with CountPlotSyntax

}

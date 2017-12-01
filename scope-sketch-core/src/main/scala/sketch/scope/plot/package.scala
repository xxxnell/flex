package sketch.scope

import sketch.scope.range._

/**
  * Licensed by Probe Technology, Inc.
  */
package object plot
  extends PlotSyntax
    with DensityPlotSyntax
    with CountPlotSyntax {

  type Record = (Range, Double)

}

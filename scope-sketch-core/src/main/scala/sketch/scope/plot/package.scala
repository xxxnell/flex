package sketch.scope

import sketch.scope.range.syntax._

/**
  * Licensed by Probe Technology, Inc.
  */
package object plot {

  object syntax
    extends PlotSyntax
      with DensityPlotSyntax
      with CountPlotSyntax

}

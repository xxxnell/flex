package sketch.scope

import sketch.scope.range.syntax._

package object plot {

  object syntax
    extends PlotSyntax
      with DensityPlotSyntax
      with CountPlotSyntax

}

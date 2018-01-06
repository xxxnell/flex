package flip

import flip.range.syntax._

package object plot {

  object syntax
    extends PlotSyntax
      with DensityPlotSyntax
      with CountPlotSyntax

}

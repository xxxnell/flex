package flip

import flip.pdf.Prim
import flip.range.syntax._

package object plot {

  /**
    * 2-tuple of (point, value)
    * */
  type Block = ((Prim, Double), (Prim, Double))

  object syntax extends PlotSyntax with RangePlotSyntax with DensityPlotSyntax with CountPlotSyntax

}

package flex

import flex.pdf.Prim
import flex.range.syntax._

package object plot {

  /**
    * 2-tuple of (point, value)
    * */
  type Block = ((Prim, Double), (Prim, Double))

  object syntax extends plot.PlotPkgSyntax

}

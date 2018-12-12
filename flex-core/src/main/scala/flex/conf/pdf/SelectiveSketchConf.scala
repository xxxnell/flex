package flex.conf.pdf

import flex.pdf.SelectiveSketch

trait SelectiveSketchConfB[+D <: SelectiveSketch[_]] extends PeriodicSketchConfB[D] {

  val rebuildThreshold: Double

}

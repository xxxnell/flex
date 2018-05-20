package flip.conf.pdf

import flip.pdf.PeriodicSketch

/**
  * A configuration for PeroidicSketch.
  * */
trait PeriodicSketchConfB[+D <: PeriodicSketch[_]] extends SketchConfB[D] {

  val startThreshold: Double

  val thresholdPeriod: Double

}

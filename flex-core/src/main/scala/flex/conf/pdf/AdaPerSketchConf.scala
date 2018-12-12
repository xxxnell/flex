package flex.conf.pdf

import flex.pdf.AdaPerSketch

/**
  * A configuration for AdaPerSketch.
  * */
trait AdaPerSketchConfB[+D <: AdaPerSketch[_]] extends AdaptiveSketchConfB[D] with PeriodicSketchConfB[D] {}

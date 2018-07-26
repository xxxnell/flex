package flip.conf.pdf

import flip.pdf.AdaPerSketch

/**
  * A configuration for AdaPerSketch.
  * */
trait AdaPerSketchConfB[+D <: AdaPerSketch[_]] extends AdaptiveSketchConfB[D] with PeriodicSketchConfB[D] {}

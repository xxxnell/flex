package flip.conf

import flip.pdf.AdaptiveSketch

/**
  * A configuration for AdaptiveSketch.
  * */
trait AdaptiveSketchConfB[+D <: AdaptiveSketch[_]] extends SketchConf {

  val queueSize: Int

}

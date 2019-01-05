package flex.conf.pdf

import flex.pdf.AdaptiveSketch

/**
 * A configuration for AdaptiveSketch.
 * */
trait AdaptiveSketchConfB[+D <: AdaptiveSketch[_]] extends SketchConf {

  val bufferSize: Int

}

package flip.conf

/**
  * A configuration for AdaptiveSketch.
  * */
trait AdaptiveSketchConf extends SketchConf {

  val queueSize: Int

}

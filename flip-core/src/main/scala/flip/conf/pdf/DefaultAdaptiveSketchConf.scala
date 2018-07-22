package flip.conf.pdf

trait DefaultAdaptiveSketchConf extends AdaptiveSketchConf with DefaultSketchConf {

  val bufferSize: Int = 30

}

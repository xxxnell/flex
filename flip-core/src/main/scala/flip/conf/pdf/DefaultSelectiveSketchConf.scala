package flip.conf.pdf

trait DefaultSelectiveSketchConf extends SelectiveSketchConf with DefaultPeriodicSketchConf {

  val rebuildThreshold: Double = 0.05

}

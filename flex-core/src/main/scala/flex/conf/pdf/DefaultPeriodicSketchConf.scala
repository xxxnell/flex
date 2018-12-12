package flex.conf.pdf

trait DefaultPeriodicSketchConf extends PeriodicSketchConf with DefaultSketchConf {

  val startThreshold: Double = 50

  val thresholdPeriod: Double = 50

}

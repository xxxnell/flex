package flip.conf

trait DefaultSketchConf
  extends AdaPerSketchConf {

  val delta: Double = 1e-5

  val mixingRatio: Double = 1

  val dataKernelWindow: Double = 1e-5

  val decayFactor: Double = 1

  val startThreshold: Double = 100

  val thresholdPeriod: Double = 100

  val queueSize: Int = 50

  val cmap: UniformCmapConf = CmapConf.uniform(200, 10, Some(-1000d), Some(1000d))

  val counter: CounterConf = CounterConf.apply(70, 2)

}

object DefaultSketchConf extends DefaultSketchConf

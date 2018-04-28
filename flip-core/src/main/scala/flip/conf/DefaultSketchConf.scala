package flip.conf

trait DefaultSketchConf extends AdaPerSketchConf {

  val delta: Double = 1e-5

  val mixingRatio: Double = 1

  val dataKernelWindow: Double = 1e-5

  val decayFactor: Double = 1

  val startThreshold: Double = 50

  val thresholdPeriod: Double = 100

  val bufferSize: Int = 50

  val cmap: UniformEqualizeCmapConf = CmapConf.uniformEqualize(20, 3, Some(-1000d), Some(1000d), 0.1)

  val counter: CounterConf = CounterConf.apply(Int.MaxValue, 1)

}

object DefaultSketchConf extends DefaultSketchConf

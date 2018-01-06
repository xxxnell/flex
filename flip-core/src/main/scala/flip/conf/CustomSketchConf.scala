package flip.conf

trait CustomSketchConf extends SketchConf

object CustomSketchConf {

  case class CustomSketchConfImpl(delta: Double,
                                  mixingRatio: Double,
                                  dataKernelWindow: Double,
                                  decayFactor: Double,
                                  cmap: CmapConf,
                                  counter: CounterConf)
    extends CustomSketchConf

  def apply(// dist
            delta: Double = DefaultSketchConf.delta,
            // sketch
            mixingRatio: Double = DefaultSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultSketchConf.dataKernelWindow,
            decayFactor: Double = DefaultSketchConf.decayFactor,
            // adaptive
            queueSize: Int = DefaultSketchConf.queueSize,
            // periodic
            startThreshold: Double = DefaultSketchConf.startThreshold,
            thresholdPeriod: Double = DefaultSketchConf.thresholdPeriod,
            // cmap
            cmapSize: Int = DefaultSketchConf.cmap.size,
            cmapNo: Int = DefaultSketchConf.cmap.no,
            cmapStart: Option[Double] = DefaultSketchConf.cmap.start,
            cmapEnd: Option[Double] = DefaultSketchConf.cmap.end,
            // counter
            counterSize: Int = DefaultSketchConf.counter.size,
            counterNo: Int = DefaultSketchConf.counter.no): CustomAdaPerSketchConf =
    AdaPerSketchConf.custom(
      delta,
      mixingRatio, dataKernelWindow, decayFactor,
      queueSize,
      startThreshold, thresholdPeriod,
      CmapConf.uniform(cmapSize, cmapNo, cmapStart, cmapEnd),
      CounterConf(counterSize, counterNo)
    )

  def simple(delta: Double,
             mixingRatio: Double,
             dataKernelWindow: Double,
             decayFactor: Double,
             cmapConf: CmapConf,
             counterConf: CounterConf): CustomSketchConf =
    CustomSketchConfImpl(delta, mixingRatio, dataKernelWindow, decayFactor, cmapConf, counterConf)

  def periodic(// sketch
               mixingRatio: Double, dataKernelWindow: Double,
               // periodic
               startThreshold: Double, thresholdPeriod: Double,
               // cmap
               cmapSize: Int, cmapNo: Int, cmapStart: Option[Double], cmapEnd: Option[Double],
               // counter
               counterSize: Int, counterNo: Int): CustomPeriodicSketchConf = ???

}

package flip.conf

trait CustomSketchConf extends SketchConf

object CustomSketchConf {

  case class CustomSketchConfImpl(mixingRatio: Double,
                                  dataKernelWindow: Double,
                                  cmap: CmapConf,
                                  counter: CounterConf)
    extends CustomSketchConf

  def apply(// sketch
            mixingRatio: Double = DefaultSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultSketchConf.dataKernelWindow,
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
      mixingRatio, dataKernelWindow,
      queueSize,
      startThreshold, thresholdPeriod,
      CmapConf.uniform(cmapSize, cmapNo, cmapStart, cmapEnd),
      CounterConf(counterSize, counterNo)
    )

  def simple(mixingRatio: Double,
             dataKernelWindow: Double,
             cmapConf: CmapConf,
             counterConf: CounterConf): CustomSketchConf =
    CustomSketchConfImpl(mixingRatio, dataKernelWindow, cmapConf, counterConf)

  def periodic(// sketch
               mixingRatio: Double, dataKernelWindow: Double,
               // periodic
               startThreshold: Double, thresholdPeriod: Double,
               // cmap
               cmapSize: Int, cmapNo: Int, cmapStart: Option[Double], cmapEnd: Option[Double],
               // counter
               counterSize: Int, counterNo: Int): CustomPeriodicSketchConf = ???

}

package sketch.scope.conf

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
            counterNo: Int = DefaultSketchConf.counter.no): CustomPeriodicSketchConf =
    PeriodicSketchConf.custom(
      mixingRatio, dataKernelWindow,
      startThreshold, thresholdPeriod,
      CmapConf.uniform(cmapSize, cmapNo, cmapStart, cmapEnd),
      CounterConf(counterSize, counterNo)
    )

  def simple(mixingRatio: Double,
             dataKernelWindow: Double,
             cmapConf: CmapConf,
             counterConf: CounterConf): CustomSketchConf =
    CustomSketchConfImpl(mixingRatio, dataKernelWindow, cmapConf, counterConf)

}

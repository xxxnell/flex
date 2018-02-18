package flip.conf

trait CustomSketchConf extends SketchConf

object CustomSketchConf {

  def apply( // dist
            delta: Double = DefaultSketchConf.delta,
            // sketch
            mixingRatio: Double = DefaultSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultSketchConf.dataKernelWindow,
            boundaryCorr: Double = DefaultSketchConf.boundaryCorrection,
            decayFactor: Double = DefaultSketchConf.decayFactor,
            // adaptive
            queueSize: Int = DefaultSketchConf.queueSize,
            // periodic
            startThreshold: Double = DefaultSketchConf.startThreshold,
            thresholdPeriod: Double = DefaultSketchConf.thresholdPeriod,
            // bind
            bindSampling: Int = DefaultSketchConf.bindSampling,
            // cmap
            cmapSize: Int = DefaultSketchConf.cmap.size,
            cmapNo: Int = DefaultSketchConf.cmap.no,
            cmapStart: Option[Double] = DefaultSketchConf.cmap.start,
            cmapEnd: Option[Double] = DefaultSketchConf.cmap.end,
            // counter
            counterSize: Int = DefaultSketchConf.counter.size,
            counterNo: Int = DefaultSketchConf.counter.no): CustomAdaPerSketchConf = {
    AdaPerSketchConf.custom(
      delta,
      mixingRatio,
      dataKernelWindow,
      boundaryCorr,
      decayFactor,
      queueSize,
      startThreshold,
      thresholdPeriod,
      bindSampling,
      CmapConf.uniform(cmapSize, cmapNo, cmapStart, cmapEnd),
      CounterConf(counterSize, counterNo)
    )
  }

  def periodic( // sketch
               mixingRatio: Double,
               dataKernelWindow: Double,
               // periodic
               startThreshold: Double,
               thresholdPeriod: Double,
               // cmap
               cmapSize: Int,
               cmapNo: Int,
               cmapStart: Option[Double],
               cmapEnd: Option[Double],
               // counter
               counterSize: Int,
               counterNo: Int): CustomPeriodicSketchConf = ???

}

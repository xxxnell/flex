package flip.conf

trait CustomAdaPerSketchConf extends CustomAdaptiveSketchConf with CustomPeriodicSketchConf with AdaPerSketchConf

object CustomAdaPerSketchConf {

  private case class CustomAdaPerSketchConfImpl( // dist
                                                delta: Double,
                                                // sketch
                                                mixingRatio: Double,
                                                dataKernelWindow: Double,
                                                decayFactor: Double,
                                                cmap: CmapConf,
                                                counter: CounterConf,
                                                // sketch: adaptive
                                                queueSize: Int,
                                                // sketch: periodic
                                                startThreshold: Double,
                                                thresholdPeriod: Double)
      extends CustomAdaPerSketchConf

  def apply( // dist
            delta: Double = DefaultSketchConf.delta,
            // sketch
            mixingRatio: Double = DefaultSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultSketchConf.dataKernelWindow,
            decayFactor: Double = DefaultSketchConf.decayFactor,
            // sketch: cmap
            cmapSize: Int = DefaultSketchConf.cmap.size,
            cmapNo: Int = DefaultSketchConf.cmap.no,
            cmapStart: Option[Double] = DefaultSketchConf.cmap.start,
            cmapEnd: Option[Double] = DefaultSketchConf.cmap.end,
            boundaryRatio: Double = DefaultSketchConf.cmap.boundaryRatio,
            // sketch: counter
            counterSize: Int = DefaultSketchConf.counter.size,
            counterNo: Int = DefaultSketchConf.counter.no,
            // sketch: adaptive
            queueSize: Int = DefaultSketchConf.queueSize,
            // sketch: periodic
            startThreshold: Double = DefaultSketchConf.startThreshold,
            thresholdPeriod: Double = DefaultSketchConf.thresholdPeriod): CustomAdaPerSketchConf =
    bare(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      CmapConf.uniformEqualize(cmapSize, cmapNo, cmapStart, cmapEnd, boundaryRatio),
      CounterConf(counterSize, counterNo),
      queueSize,
      startThreshold,
      thresholdPeriod
    )

  def bare( // dist
           delta: Double,
           // sketch
           mixingRatio: Double,
           dataKernelWindow: Double,
           decayFactor: Double,
           cmap: CmapConf,
           counter: CounterConf,
           // sketch: adaptive
           queueSize: Int,
           // sketch: periodic
           startThreshold: Double,
           thresholdPeriod: Double): CustomAdaPerSketchConf =
    CustomAdaPerSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      cmap,
      counter,
      queueSize,
      startThreshold,
      thresholdPeriod
    )

}

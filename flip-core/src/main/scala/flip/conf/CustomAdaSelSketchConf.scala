package flip.conf

trait CustomAdaSelSketchConf extends CustomAdaptiveSketchConf with CustomPeriodicSketchConf with AdaSelSketchConf

object CustomAdaSelSketchConf {

  private case class CustomAdaPerSketchConfImpl( // dist
                                                delta: Double,
                                                // sketch
                                                mixingRatio: Double,
                                                dataKernelWindow: Double,
                                                decayFactor: Double,
                                                cmap: CmapConf,
                                                counter: CounterConf,
                                                // sketch: adaptive
                                                bufferSize: Int,
                                                // sketch: periodic
                                                startThreshold: Double,
                                                thresholdPeriod: Double,
                                                // sketch: selective
                                                rebuildThreshold: Double)
      extends CustomAdaSelSketchConf

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
            bufferSize: Int = DefaultSketchConf.bufferSize,
            // sketch: periodic
            startThreshold: Double = DefaultSketchConf.startThreshold,
            thresholdPeriod: Double = DefaultSketchConf.thresholdPeriod,
            // sketch: selective
            rebuildThreshold: Double = 1): CustomAdaSelSketchConf =
    bare(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      CmapConf.uniformEqualize(cmapSize, cmapNo, cmapStart, cmapEnd, boundaryRatio),
      CounterConf(counterSize, counterNo),
      bufferSize,
      startThreshold,
      thresholdPeriod,
      rebuildThreshold
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
           bufferSize: Int,
           // sketch: periodic
           startThreshold: Double,
           thresholdPeriod: Double,
           // sketch: selective
           rebuildThreshold: Double): CustomAdaSelSketchConf =
    CustomAdaPerSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      cmap,
      counter,
      bufferSize,
      startThreshold,
      thresholdPeriod,
      rebuildThreshold
    )

}

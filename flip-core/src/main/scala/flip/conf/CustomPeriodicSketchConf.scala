package flip.conf

trait CustomPeriodicSketchConf extends CustomSketchConf with PeriodicSketchConf

object CustomPeriodicSketchConf {

  private case class PeriodicSketchConfImpl( // dist
                                            delta: Double,
                                            // sketch
                                            mixingRatio: Double,
                                            dataKernelWindow: Double,
                                            decayFactor: Double,
                                            cmap: CmapConf,
                                            counter: CounterConf,
                                            // sketch: periodic
                                            startThreshold: Double,
                                            thresholdPeriod: Double)
      extends CustomPeriodicSketchConf

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
            // sketch: periodic
            startThreshold: Double = DefaultSketchConf.startThreshold,
            thresholdPeriod: Double = DefaultSketchConf.thresholdPeriod): CustomPeriodicSketchConf =
    bare(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      CmapConf.uniformEqualize(cmapSize, cmapNo, cmapStart, cmapEnd, boundaryRatio),
      CounterConf(counterSize, counterNo),
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
           // sketch: periodic
           startThreshold: Double,
           thresholdPeriod: Double): CustomPeriodicSketchConf =
    PeriodicSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      cmap,
      counter,
      startThreshold,
      thresholdPeriod
    )

}

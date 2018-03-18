package flip.conf

object CustomSketchConf {

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
            thresholdPeriod: Double = DefaultSketchConf.thresholdPeriod): AdaPerSketchConf = {
    AdaPerSketchConf.custom(
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
               counterNo: Int): PeriodicSketchConf = ???

}

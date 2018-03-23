package flip.conf

trait CustomSketchConf extends CustomDataBinningDistConf with SketchConf

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
            thresholdPeriod: Double = DefaultSketchConf.thresholdPeriod): CustomSketchConf = {
    CustomAdaPerSketchConf.bare(
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

}

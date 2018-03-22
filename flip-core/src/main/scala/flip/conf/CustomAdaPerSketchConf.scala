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

  def apply(delta: Double,
            mixingRatio: Double,
            dataKernelWindow: Double,
            decayFactor: Double,
            cmap: CmapConf,
            counter: CounterConf,
            queueSize: Int,
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

package sketch.scope.conf

/**
  * A configuration for AdaPerSketch.
  * */
trait AdaPerSketchConf
  extends AdaptiveSketchConf
    with PeriodicSketchConf {

}

trait CustomAdaPerSketchConf extends AdaPerSketchConf with CustomSketchConf

object AdaPerSketchConf {

  private case class AdaPerSketchConfImpl(delta: Double,
                                          mixingRatio: Double,
                                          dataKernelWindow: Double,
                                          decayFactor: Double,
                                          queueSize: Int,
                                          startThreshold: Double,
                                          thresholdPeriod: Double,
                                          cmap: CmapConf,
                                          counter: CounterConf)
    extends CustomAdaPerSketchConf

  def custom(delta: Double,
             mixingRatio: Double,
             dataKernelWindow: Double,
             decayFactor: Double,
             queueSize: Int,
             startThreshold: Double,
             thresholdPeriod: Double,
             cmap: CmapConf,
             counter: CounterConf): CustomAdaPerSketchConf =
    AdaPerSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      queueSize,
      startThreshold,
      thresholdPeriod,
      cmap,
      counter
    )

}

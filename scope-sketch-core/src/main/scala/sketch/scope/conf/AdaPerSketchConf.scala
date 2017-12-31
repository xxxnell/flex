package sketch.scope.conf

trait AdaPerSketchConf
  extends AdaptiveSketchConf
    with PeriodicSketchConf {

}

trait CustomAdaPerSketchConf extends AdaPerSketchConf with CustomSketchConf

object AdaPerSketchConf {

  private case class AdaPerSketchConfImpl(mixingRatio: Double,
                                          dataKernelWindow: Double,
                                          queueSize: Int,
                                          startThreshold: Double,
                                          thresholdPeriod: Double,
                                          cmap: CmapConf,
                                          counter: CounterConf)
    extends CustomAdaPerSketchConf

  def custom(mixingRatio: Double,
             dataKernelWindow: Double,
             queueSize: Int,
             startThreshold: Double,
             thresholdPeriod: Double,
             cmap: CmapConf,
             counter: CounterConf): CustomAdaPerSketchConf =
    AdaPerSketchConfImpl(
      mixingRatio,
      dataKernelWindow,
      queueSize,
      startThreshold,
      thresholdPeriod,
      cmap,
      counter
    )

}

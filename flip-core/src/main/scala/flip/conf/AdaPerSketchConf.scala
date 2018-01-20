package flip.conf

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
                                          boundaryCorrection: Double,
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
             boundaryCorrection: Double,
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
      boundaryCorrection,
      decayFactor,
      queueSize,
      startThreshold,
      thresholdPeriod,
      cmap,
      counter
    )

}

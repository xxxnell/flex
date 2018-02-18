package flip.conf

/**
  * A configuration for PeroidicSketch.
  * */
trait PeriodicSketchConf extends SketchConf {

  val startThreshold: Double

  val thresholdPeriod: Double

}

trait CustomPeriodicSketchConf extends PeriodicSketchConf with CustomSketchConf

object PeriodicSketchConf {

  private case class PeriodicSketchConfImpl(delta: Double,
                                            mixingRatio: Double,
                                            dataKernelWindow: Double,
                                            boundaryCorrection: Double,
                                            decayFactor: Double,
                                            startThreshold: Double,
                                            thresholdPeriod: Double,
                                            bindSampling: Int,
                                            cmap: CmapConf,
                                            counter: CounterConf)
      extends CustomPeriodicSketchConf

  def custom(delta: Double,
             mixingRatio: Double,
             dataKernelWindow: Double,
             boundaryCorrection: Double,
             decayFactor: Double,
             startThreshold: Double,
             thresholdPeriod: Double,
             bindSampling: Int,
             cmap: CmapConf,
             counter: CounterConf): CustomPeriodicSketchConf =
    PeriodicSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      boundaryCorrection,
      decayFactor,
      startThreshold,
      thresholdPeriod,
      bindSampling,
      cmap,
      counter
    )

}

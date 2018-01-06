package sketch.scope.conf

/**
  * A configuration for PeroidicSketch.
  * */
trait PeriodicSketchConf extends SketchConf {

  val startThreshold: Double

  val thresholdPeriod: Double

}

trait CustomPeriodicSketchConf extends PeriodicSketchConf with CustomSketchConf

object PeriodicSketchConf {

  private case class CustomPeriodicSketchConfImpl(delta: Double,
                                                  mixingRatio: Double,
                                                  dataKernelWindow: Double,
                                                  decayFactor: Double,
                                                  startThreshold: Double,
                                                  thresholdPeriod: Double,
                                                  cmap: CmapConf,
                                                  counter: CounterConf)
    extends CustomPeriodicSketchConf

  def custom(delta: Double,
             mixingRatio: Double,
             dataKernelWindow: Double,
             decayFactor: Double,
             startThreshold: Double,
             thresholdPeriod: Double,
             cmap: CmapConf,
             counter: CounterConf): CustomPeriodicSketchConf =
    CustomPeriodicSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      startThreshold,
      thresholdPeriod,
      cmap,
      counter
    )

}

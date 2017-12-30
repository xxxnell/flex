package sketch.scope.conf

trait PeriodicSketchConf extends SketchConf {

  val startThreshold: Double

  val thresholdPeriod: Double

}

trait CustomPeriodicSketchConf extends PeriodicSketchConf with CustomSketchConf

object PeriodicSketchConf {

  private case class CustomPeriodicSketchConfImpl(mixingRatio: Double,
                                              dataKernelWindow: Double,
                                              startThreshold: Double,
                                              thresholdPeriod: Double,
                                              cmap: CmapConf,
                                              counter: CounterConf)
    extends CustomPeriodicSketchConf

  def custom(mixingRatio: Double,
             dataKernelWindow: Double,
             startThreshold: Double,
             thresholdPeriod: Double,
             cmap: CmapConf,
             counter: CounterConf): CustomPeriodicSketchConf =
    CustomPeriodicSketchConfImpl(
      mixingRatio,
      dataKernelWindow,
      startThreshold,
      thresholdPeriod,
      cmap,
      counter
    )

}

package flip.conf

import flip.pdf.AdaPerSketch

/**
  * A configuration for AdaPerSketch.
  * */
trait AdaPerSketchConfB[+D <: AdaPerSketch[_]] extends AdaptiveSketchConfB[D] with PeriodicSketchConfB[D] {}

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
                                          bindSampling: Int,
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
             bindSampling: Int,
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
      bindSampling,
      cmap,
      counter
    )

}

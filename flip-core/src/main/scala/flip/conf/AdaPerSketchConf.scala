package flip.conf

import flip.pdf.AdaPerSketch

/**
  * A configuration for AdaPerSketch.
  * */
trait AdaPerSketchConfB[+D <: AdaPerSketch[_]] extends AdaptiveSketchConfB[D] with PeriodicSketchConfB[D] {}

object AdaPerSketchConf {

  private case class AdaPerSketchConfImpl( // dist
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
      extends AdaPerSketchConf

  def custom(delta: Double,
             mixingRatio: Double,
             dataKernelWindow: Double,
             decayFactor: Double,
             cmap: CmapConf,
             counter: CounterConf,
             queueSize: Int,
             startThreshold: Double,
             thresholdPeriod: Double): AdaPerSketchConf =
    AdaPerSketchConfImpl(
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

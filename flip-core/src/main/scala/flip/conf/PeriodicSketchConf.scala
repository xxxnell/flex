package flip.conf

import flip.pdf.PeriodicSketch

/**
  * A configuration for PeroidicSketch.
  * */
trait PeriodicSketchConfB[+D <: PeriodicSketch[_]] extends SketchConfB[D] {

  val startThreshold: Double

  val thresholdPeriod: Double

}

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
      extends PeriodicSketchConf

  def custom(delta: Double,
             mixingRatio: Double,
             dataKernelWindow: Double,
             boundaryCorrection: Double,
             decayFactor: Double,
             startThreshold: Double,
             thresholdPeriod: Double,
             bindSampling: Int,
             cmap: CmapConf,
             counter: CounterConf): PeriodicSketchConf =
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

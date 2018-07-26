package flip.conf.pdf

import flip.conf.cmap.CmapConf
import flip.conf.counter.CounterConf

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
                                                bufferSize: Int,
                                                // sketch: periodic
                                                startThreshold: Double,
                                                thresholdPeriod: Double)
      extends CustomAdaPerSketchConf

  def apply( // dist
            delta: Double = DefaultAdaPerSketchConf.delta,
            // cmap
            cmapSize: Int = DefaultAdaPerSketchConf.cmap.size,
            cmapNo: Int = DefaultAdaPerSketchConf.cmap.no,
            cmapStart: Option[Double] = DefaultAdaPerSketchConf.cmap.start,
            cmapEnd: Option[Double] = DefaultAdaPerSketchConf.cmap.end,
            boundaryRatio: Double = DefaultAdaPerSketchConf.cmap.boundaryRatio,
            // counter
            counterSize: Int = DefaultAdaPerSketchConf.counter.size,
            counterNo: Int = DefaultAdaPerSketchConf.counter.no,
            // sketch
            mixingRatio: Double = DefaultAdaPerSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultAdaPerSketchConf.dataKernelWindow,
            decayFactor: Double = DefaultAdaPerSketchConf.decayFactor,
            // adaptive sketch
            bufferSize: Int = DefaultAdaPerSketchConf.bufferSize,
            // periodic sketch
            startThreshold: Double = DefaultAdaPerSketchConf.startThreshold,
            thresholdPeriod: Double = DefaultAdaPerSketchConf.thresholdPeriod): CustomAdaPerSketchConf =
    bare(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      CmapConf.uniformEqualize(cmapSize, cmapNo, cmapStart, cmapEnd, boundaryRatio),
      CounterConf(counterSize, counterNo),
      bufferSize,
      startThreshold,
      thresholdPeriod
    )

  def bare( // dist
           delta: Double,
           // sketch
           mixingRatio: Double,
           dataKernelWindow: Double,
           decayFactor: Double,
           cmap: CmapConf,
           counter: CounterConf,
           // sketch: adaptive
           bufferSize: Int,
           // sketch: periodic
           startThreshold: Double,
           thresholdPeriod: Double): CustomAdaPerSketchConf =
    CustomAdaPerSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      cmap,
      counter,
      bufferSize,
      startThreshold,
      thresholdPeriod
    )

  def default: AdaPerSketchConf = DefaultAdaPerSketchConf

}

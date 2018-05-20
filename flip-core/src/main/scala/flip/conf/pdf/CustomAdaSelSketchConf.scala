package flip.conf.pdf

import flip.conf._
import flip.conf.cmap.CmapConf
import flip.conf.counter.CounterConf

trait CustomAdaSelSketchConf extends CustomAdaptiveSketchConf with CustomPeriodicSketchConf with AdaSelSketchConf

object CustomAdaSelSketchConf {

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
                                                thresholdPeriod: Double,
                                                // sketch: selective
                                                rebuildThreshold: Double)
      extends CustomAdaSelSketchConf

  def apply( // dist
            delta: Double = DefaultAdaSelSketchConf.delta,
            // sketch
            mixingRatio: Double = DefaultAdaSelSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultAdaSelSketchConf.dataKernelWindow,
            decayFactor: Double = DefaultAdaSelSketchConf.decayFactor,
            // sketch: cmap
            cmapSize: Int = DefaultAdaSelSketchConf.cmap.size,
            cmapNo: Int = DefaultAdaSelSketchConf.cmap.no,
            cmapStart: Option[Double] = DefaultAdaSelSketchConf.cmap.start,
            cmapEnd: Option[Double] = DefaultAdaSelSketchConf.cmap.end,
            boundaryRatio: Double = DefaultAdaSelSketchConf.cmap.boundaryRatio,
            // sketch: counter
            counterSize: Int = DefaultAdaSelSketchConf.counter.size,
            counterNo: Int = DefaultAdaSelSketchConf.counter.no,
            // sketch: adaptive
            bufferSize: Int = DefaultAdaSelSketchConf.bufferSize,
            // sketch: periodic
            startThreshold: Double = DefaultAdaSelSketchConf.startThreshold,
            thresholdPeriod: Double = DefaultAdaSelSketchConf.thresholdPeriod,
            // sketch: selective
            rebuildThreshold: Double = DefaultAdaSelSketchConf.rebuildThreshold): CustomAdaSelSketchConf =
    bare(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      CmapConf.uniformEqualize(cmapSize, cmapNo, cmapStart, cmapEnd, boundaryRatio),
      CounterConf(counterSize, counterNo),
      bufferSize,
      startThreshold,
      thresholdPeriod,
      rebuildThreshold
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
           thresholdPeriod: Double,
           // sketch: selective
           rebuildThreshold: Double): CustomAdaSelSketchConf =
    CustomAdaPerSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      cmap,
      counter,
      bufferSize,
      startThreshold,
      thresholdPeriod,
      rebuildThreshold
    )

  def default: AdaSelSketchConf = DefaultAdaSelSketchConf

}

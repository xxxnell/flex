package flip.conf.pdf

import flip.conf.cmap.CmapConf
import flip.conf.counter.CounterConf

trait CustomSimpleSketchConf extends SimpleSketchConf with CustomSketchConf

object CustomSimpleSketchConf {

  private case class CustomSimpleSketchConfImpl(delta: Double,
                                                mixingRatio: Double,
                                                dataKernelWindow: Double,
                                                decayFactor: Double,
                                                cmap: CmapConf,
                                                counter: CounterConf)
      extends CustomSimpleSketchConf

  def apply( // dist
            delta: Double = DefaultAdaPerSketchConf.delta,
            // sketch
            mixingRatio: Double = DefaultAdaPerSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultAdaPerSketchConf.dataKernelWindow,
            decayFactor: Double = DefaultAdaPerSketchConf.decayFactor,
            // sketch: cmap
            binNo: Int = DefaultAdaPerSketchConf.cmap.size,
            start: Double,
            end: Double,
            // sketch: counter
            counterSize: Int = DefaultAdaPerSketchConf.counter.size,
            counterNo: Int = DefaultAdaPerSketchConf.counter.no): CustomSimpleSketchConf = {
    val cmapConf = CmapConf.uniformEqualize(binNo, 1, Some(start), Some(end), 1)
    val counterConf = CounterConf(counterSize, counterNo)
    bare(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      cmapConf,
      counterConf
    )
  }

  def bare(delta: Double,
           mixingRatio: Double,
           dataKernelWindow: Double,
           decayFactor: Double,
           cmapConf: CmapConf,
           counterConf: CounterConf): CustomSimpleSketchConf =
    CustomSimpleSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      cmapConf,
      counterConf
    )

}

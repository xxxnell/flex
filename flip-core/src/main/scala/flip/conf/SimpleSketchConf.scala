package flip.conf

trait SimpleSketchConf extends SketchConf

trait CustomSimpleSketchConf extends SimpleSketchConf with CustomSketchConf

object SimpleSketchConf {

  private case class SimpleSketchConfImpl(delta: Double,
                                          mixingRatio: Double,
                                          dataKernelWindow: Double,
                                          boundaryCorrection: Double,
                                          decayFactor: Double,
                                          bindSampling: Int,
                                          cmap: CmapConf,
                                          counter: CounterConf)
    extends CustomSimpleSketchConf

  def apply(// dist
            delta: Double = DefaultSketchConf.delta,
            // deepUpdate
            mixingRatio: Double = DefaultSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultSketchConf.dataKernelWindow,
            boundaryCorr: Double = DefaultSketchConf.boundaryCorrection,
            decayFactor: Double = DefaultSketchConf.decayFactor,
            // bind
            bindSampling: Int = DefaultSketchConf.bindSampling,
            // cmap
            binNo: Int,
            start: Double,
            end: Double,
            // counter
            counterSize: Int = DefaultSketchConf.counter.size,
            counterNo: Int = DefaultSketchConf.counter.no): CustomSimpleSketchConf = {
    val cmapConf = CmapConf.uniform(binNo, 1, start, end)
    val counterConf = CounterConf(counterSize, counterNo)
    SimpleSketchConf.custom(
      delta,
      mixingRatio, dataKernelWindow, decayFactor, boundaryCorr,
      bindSampling,
      cmapConf, counterConf
    )
  }

  def custom(delta: Double,
             mixingRatio: Double,
             dataKernelWindow: Double,
             boundaryCorr: Double,
             decayFactor: Double,
             bindSampling: Int,
             cmapConf: CmapConf,
             counterConf: CounterConf): CustomSimpleSketchConf =
    SimpleSketchConfImpl(
      delta,
      mixingRatio, dataKernelWindow, decayFactor, boundaryCorr,
      bindSampling,
      cmapConf, counterConf
    )

}
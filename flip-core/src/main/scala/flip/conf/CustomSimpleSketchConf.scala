package flip.conf

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
            delta: Double = DefaultSketchConf.delta,
            // sketch
            mixingRatio: Double = DefaultSketchConf.mixingRatio,
            dataKernelWindow: Double = DefaultSketchConf.dataKernelWindow,
            decayFactor: Double = DefaultSketchConf.decayFactor,
            // sketch: cmap
            binNo: Int,
            start: Double,
            end: Double,
            // sketch: counter
            counterSize: Int = DefaultSketchConf.counter.size,
            counterNo: Int = DefaultSketchConf.counter.no): CustomSimpleSketchConf = {
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

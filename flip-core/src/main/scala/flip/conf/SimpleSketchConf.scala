package flip.conf

trait SimpleSketchConf extends SketchConf

object SimpleSketchConf {

  private case class SimpleSketchConfImpl(delta: Double,
                                          mixingRatio: Double,
                                          dataKernelWindow: Double,
                                          decayFactor: Double,
                                          cmap: CmapConf,
                                          counter: CounterConf)
      extends SimpleSketchConf

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
            counterNo: Int = DefaultSketchConf.counter.no): SimpleSketchConf = {
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
             counterConf: CounterConf): SimpleSketchConf =
    SimpleSketchConfImpl(
      delta,
      mixingRatio,
      dataKernelWindow,
      decayFactor,
      cmapConf,
      counterConf
    )

}

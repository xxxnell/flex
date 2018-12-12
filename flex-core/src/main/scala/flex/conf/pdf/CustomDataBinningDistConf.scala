package flex.conf.pdf

import flex.conf.cmap.CmapConf
import flex.conf.counter.CounterConf

trait CustomDataBinningDistConf extends CustomSamplingDistConf with DataBinningDistConf

object CustomDataBinningDistConf {

  private case class CustomDataBinningDistConfImpl(delta: Double, cmap: CmapConf, counter: CounterConf)
      extends CustomDataBinningDistConf

  def apply( // dist
            delta: Double = DefaultAdaPerSketchConf.delta,
            // cmap
            cmapSize: Int = DefaultAdaSelSketchConf.cmap.size,
            cmapNo: Int = DefaultAdaSelSketchConf.cmap.no,
            cmapStart: Option[Double] = DefaultAdaSelSketchConf.cmap.start,
            cmapEnd: Option[Double] = DefaultAdaSelSketchConf.cmap.end,
            boundaryRatio: Double = DefaultAdaSelSketchConf.cmap.boundaryRatio,
            // counter
            counterSize: Int = DefaultAdaSelSketchConf.counter.size,
            counterNo: Int = DefaultAdaSelSketchConf.counter.no): CustomDataBinningDistConf =
    bare(
      delta,
      CmapConf.uniformEqualize(cmapSize, cmapNo, cmapStart, cmapEnd, boundaryRatio),
      CounterConf(counterSize, counterNo)
    )

  def bare(delta: Double, cmap: CmapConf, counter: CounterConf): CustomDataBinningDistConf =
    CustomDataBinningDistConfImpl(delta, cmap, counter)

}

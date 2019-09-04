package flex.conf.pdf

import flex.conf.cmap.{ CmapConf, UniformEqualizeCmapConf }
import flex.conf.counter.CounterConf

trait DefaultDataBinningDistConf extends DataBinningDistConf {

  val delta: Double = 1e-5

  val cmap: UniformEqualizeCmapConf = CmapConf.uniformEqualize(25, 3, Some(-1000d), Some(1000d), 0.1)

  val counter: CounterConf = CounterConf.apply(Int.MaxValue, 1)

}

object DefaultDataBinningDistConf extends DefaultDataBinningDistConf

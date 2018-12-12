package flex.conf.pdf

import flex.conf.cmap.{CmapConf, UniformEqualizeCmapConf}
import flex.conf.counter.CounterConf

trait DefaultSketchConf extends SketchConf with DefaultDataBinningDistConf {

  val mixingRatio: Double = 1

  val dataKernelWindow: Double = 1e-5

  val decayFactor: Double = 2.5

}

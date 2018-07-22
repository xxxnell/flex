package flip.conf.pdf

import flip.conf.cmap.{CmapConf, UniformEqualizeCmapConf}
import flip.conf.counter.CounterConf

trait DefaultSketchConf extends SketchConf with DefaultDataBinningDistConf {

  val mixingRatio: Double = 1

  val dataKernelWindow: Double = 1e-5

  val decayFactor: Double = 2.5

}

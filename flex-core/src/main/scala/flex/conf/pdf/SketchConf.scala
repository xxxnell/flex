package flex.conf.pdf

import flex.conf.cmap.CmapConf
import flex.conf.counter.CounterConf
import flex.pdf.Sketch

trait SketchConfB[+D <: Sketch[_]] extends DataBinningDistConfB[D] {
  // structures
  val decayFactor: Double
  // deepUpdate
  val mixingRatio: Double
  val dataKernelWindow: Double
}

object SketchConf {

  def default: SketchConf = DefaultAdaPerSketchConf

}

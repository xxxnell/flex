package flip.conf.pdf

import flip.conf.cmap.CmapConf
import flip.conf.counter.CounterConf
import flip.pdf.Sketch

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

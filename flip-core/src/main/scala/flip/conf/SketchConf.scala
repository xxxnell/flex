package flip.conf

import flip.pdf.Sketch

trait SketchConfB[+D <: Sketch[_]] extends DataBinningDistConfB[D] {
  // structures
  val decayFactor: Double
  val cmap: CmapConf
  val counter: CounterConf
  // deepUpdate
  val mixingRatio: Double
  val dataKernelWindow: Double
}



object SketchConf {

  def default: SketchConf = DefaultSketchConf

}

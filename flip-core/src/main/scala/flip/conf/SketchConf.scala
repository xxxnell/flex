package flip.conf

trait SketchConf extends DataBinningDistConf {
  // deepUpdate
  val mixingRatio: Double
  val dataKernelWindow: Double
  val boundaryCorrection: Double
  val decayFactor: Double
  // bind
  val bindSampling: Int
  // structures
  val cmap: CmapConf
  val counter: CounterConf
}

object SketchConf {

  def default: SketchConf = DefaultSketchConf

}

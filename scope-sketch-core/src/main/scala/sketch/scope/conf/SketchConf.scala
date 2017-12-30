package sketch.scope.conf

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchConf extends DataBinningDistConf {
  val cmap: CmapConf
  val counter: CounterConf
}

object SketchConf {

  def default: SketchConf = DefaultSketchConf

}

package sketch.scope.conf

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchConf {
  val cmap: CmapConf
  val counter: CounterConf
}

object SketchConf {

  case class SketchConfImpl(cmap: CmapConf,
                            counter: CounterConf) extends SketchConf

  def default: SketchConf = DefaultSketchConf

  def apply(cmapConf: CmapConf,
            counterConf: CounterConf): SketchConf =
    SketchConfImpl(cmapConf, counterConf)

}

package sketch.scope.conf

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchConf extends DataBinningDistConf {
  val cmap: CmapConf
  val counter: CounterConf
}

trait CustomSketchConf extends SketchConf

object SketchConf {

  case class CustomSketchConfImpl(cmap: CmapConf,
                                  counter: CounterConf) extends CustomSketchConf

  def default: SketchConf = DefaultSketchConf

  def apply(cmapConf: CmapConf,
            counterConf: CounterConf): CustomSketchConf =
    custom(cmapConf, counterConf)

  def custom(cmapConf: CmapConf,
             counterConf: CounterConf): CustomSketchConf =
    CustomSketchConfImpl(cmapConf, counterConf)


}

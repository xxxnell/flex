package sketch.scope.conf

trait CustomSketchConf extends SketchConf

object CustomSketchConf {

  case class CustomSketchConfImpl(cmap: CmapConf,
                                  counter: CounterConf) extends CustomSketchConf

  def apply(cmapConf: CmapConf,
            counterConf: CounterConf): CustomSketchConf =
    custom(cmapConf, counterConf)

  def apply(cmapSize: Int, cmapNo: Int,
            counterSize: Int, counterNo: Int): CustomSketchConf =
    custom(
      CmapConf.uniform(cmapSize, cmapNo, None, None),
      CounterConf(counterSize, counterNo)
    )

  def apply(cmapSize: Int, cmapNo: Int, cmapMin: Double, cmapMax: Double,
            counterSize: Int, counterNo: Int): CustomSketchConf =
    custom(
      CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
      CounterConf(counterSize, counterNo)
    )

  def custom(cmapConf: CmapConf,
             counterConf: CounterConf): CustomSketchConf =
    CustomSketchConfImpl(cmapConf, counterConf)

}

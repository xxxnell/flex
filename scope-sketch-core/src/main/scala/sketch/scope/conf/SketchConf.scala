package sketch.scope.conf

/**
  * Licensed by Probe Technology, Inc.
  */
trait SketchConf {
  // cmap
  val cmapSize: Int
  val cmapNo: Int
  val cmapMin: Double
  val cmapMax: Double
  // hcounter
  val counterSize: Int
  val counterNo: Int
}

object SketchConf {

  case class SketchConfImpl(cmapSize: Int,
                            cmapNo: Int,
                            cmapMin: Double,
                            cmapMax: Double,
                            counterSize: Int,
                            counterNo: Int) extends SketchConf

  def default: SketchConf = DefaultSketchConf

  def apply(cmapSize: Int, cmapNo: Int, cmapMin: Double, cmapMax: Double,
            counterSize: Int, counterNo: Int): SketchConf =
    SketchConfImpl(
      cmapSize, cmapNo, cmapMin, cmapMax,
      counterSize, counterNo
    )

}

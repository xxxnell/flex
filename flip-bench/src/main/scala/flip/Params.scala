package flip

object Params {

  val cmapSizes: List[Int] = (SketchConf.default.cmap.size :: List(100)).distinct.sorted

  val cmapNos: List[Int] = (SketchConf.default.cmap.no :: List(10)).distinct.sorted

  val bufferSizes: List[Int] = (SketchConf.default.bufferSize :: List(100)).distinct.sorted

  val decayFactors: List[Double] = (SketchConf.default.decayFactor :: List(2.0)).distinct.sorted

  val rebuildThresholds: List[Double] = (SketchConf.default.rebuildThreshold :: List(1.0)).distinct.sorted

  def toConfs: List[SketchConf] =
    for {
      cmapSize <- cmapSizes
      cmapNo <- cmapNos
      bufferSize <- bufferSizes
      decayFactor <- decayFactors
      rebuildThreshold <- rebuildThresholds
    } yield
      SketchConf(
        cmapSize = cmapSize,
        cmapNo = cmapNo,
        cmapStart = Some(-20),
        cmapEnd = Some(20),
        bufferSize = bufferSize,
        thresholdPeriod = bufferSize,
        decayFactor = decayFactor,
        rebuildThreshold = rebuildThreshold
      )

}

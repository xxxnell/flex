package flip

import flip.implicits._

object Params {

  val cmapSizes = List(20)

  val cmapNos = List(3, 10)

  val bufferSizes = List(10, 100)

  val decayFactors = List(1.0, 2.0)

  val rebuildThresholds = List(0.0, 0.2)

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



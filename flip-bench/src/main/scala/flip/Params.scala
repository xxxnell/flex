package flip

trait Params {

  val cmapSizes: List[Int]

  val cmapNos: List[Int]

  val bufferSizes: List[Int]

  val decayFactors: List[Double]

  val rebuildThresholds: List[Double]

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

object Params {

  private case class ParamImpl(cmapSizes: List[Int],
                               cmapNos: List[Int],
                               bufferSizes: List[Int],
                               decayFactors: List[Double],
                               rebuildThresholds: List[Double])
      extends Params

  def apply(cmapSizes: List[Int],
            cmapNos: List[Int],
            bufferSizes: List[Int],
            decayFactors: List[Double],
            rebuildThresholds: List[Double]): Params =
    ParamImpl(cmapSizes, cmapNos, bufferSizes, decayFactors, rebuildThresholds)

  def default: Params =
    apply(defaultCmapSizes, defaultCmapNos, defaultBufferSizes, defaultDecayFactors, defaultRebuildThresholds)

  def reducedBy(i: Int): Params = {
    def reduce[A](as: List[A], i: Int): List[A] = as.sliding(i, i).toList.flatMap(ps => ps.headOption)
    apply(
      reduce(defaultCmapSizes, i),
      reduce(defaultCmapNos, i),
      reduce(defaultBufferSizes, i),
      reduce(defaultDecayFactors, i),
      reduce(defaultRebuildThresholds, i))
  }

  val defaultCmapSizes: List[Int] = (10 :: 25 :: 40 :: 55 :: 100 :: Nil).distinct.sorted

  val defaultCmapNos: List[Int] = (2 :: 3 :: 5 :: 10 :: Nil).distinct.sorted

  val defaultBufferSizes: List[Int] = (10 :: 30 :: 70 :: 120 :: 150 :: Nil).distinct.sorted

  val defaultDecayFactors: List[Double] = (0.2 :: 0.5 :: 1.0 :: 2.0 :: 2.5 :: 5.0 :: Nil).distinct.sorted

  val defaultRebuildThresholds: List[Double] = (0.01 :: 0.09 :: 0.3 :: 0.5 :: 0.7 :: Nil).distinct.sorted

}

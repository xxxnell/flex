package flip.conf.pdf

trait IcdfSamplingConf {

  def size: Int

}

object IcdfSamplingConf {

  val default: EqualizeIcdfSamplingConf = EqualizeIcdfSamplingConf.default

  def apply(size: Int = default.size, boundaryRatio: Double = default.boundaryRatio): EqualizeIcdfSamplingConf =
    EqualizeIcdfSamplingConf(size, boundaryRatio)

}

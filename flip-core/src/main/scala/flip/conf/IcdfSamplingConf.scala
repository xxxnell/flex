package flip.conf

trait IcdfSamplingConf

object IcdfSamplingConf {

  val default: EqualizedIcdfSamplingConf = EqualizedIcdfSamplingConf.default

  def apply(size: Int = default.size, boundaryRatio: Double = default.boundaryRatio): EqualizedIcdfSamplingConf =
    EqualizedIcdfSamplingConf(size, boundaryRatio)

}

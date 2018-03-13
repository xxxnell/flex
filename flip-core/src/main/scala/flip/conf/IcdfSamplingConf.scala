package flip.conf

trait IcdfSamplingConf

object IcdfSamplingConf {

  val default: IcdfSamplingConf = EqualizedIcdfSamplingConf(20, 0.05)

}

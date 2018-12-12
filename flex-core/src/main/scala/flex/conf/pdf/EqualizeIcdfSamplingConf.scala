package flex.conf.pdf

trait EqualizeIcdfSamplingConf extends IcdfSamplingConf {

  def boundaryRatio: Double

}

object EqualizeIcdfSamplingConf {

  private case class EqualizeIcdfSamplingConfImpl(size: Int, boundaryRatio: Double) extends EqualizeIcdfSamplingConf

  def apply(size: Int, boundaryRatio: Double): EqualizeIcdfSamplingConf =
    bare(size, boundaryRatio)

  def bare(size: Int, boundaryRatio: Double): EqualizeIcdfSamplingConf =
    EqualizeIcdfSamplingConfImpl(size, boundaryRatio)

  val default: EqualizeIcdfSamplingConf = bare(20, 0.05)

}

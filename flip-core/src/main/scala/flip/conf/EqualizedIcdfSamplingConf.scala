package flip.conf

trait EqualizedIcdfSamplingConf extends IcdfSamplingConf {

  def size: Int

  def boundaryRatio: Double

}

object EqualizedIcdfSamplingConf {

  private case class EqualizedIcdfSamplingConfImpl(size: Int, boundaryRatio: Double) extends EqualizedIcdfSamplingConf

  def apply(size: Int, boundaryRatio: Double): EqualizedIcdfSamplingConf =
    bare(size, boundaryRatio)

  def bare(size: Int, boundaryRatio: Double): EqualizedIcdfSamplingConf =
    EqualizedIcdfSamplingConfImpl(size, boundaryRatio)

}

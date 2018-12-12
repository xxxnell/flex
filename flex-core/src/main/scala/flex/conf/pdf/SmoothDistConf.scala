package flex.conf.pdf

import flex.pdf.SmoothDist

trait SmoothDistConfB[D <: SmoothDist[_]] extends DistConfB[D] {

  val sampling: IcdfSamplingConf

}

object DefaultSmoothDistConf extends SmoothDistConf {

  val delta: Double = DefaultAdaPerSketchConf.delta

  val sampling: EqualizeIcdfSamplingConf = IcdfSamplingConf.default

}

object SmoothDistConf {

  private case class SmoothDistConfImpl(delta: Double, sampling: IcdfSamplingConf) extends SmoothDistConf

  def apply(delta: Double = DefaultSmoothDistConf.delta,
            samplingSize: Int = DefaultSmoothDistConf.sampling.size,
            samplingBoundaryRatio: Double = DefaultSmoothDistConf.sampling.boundaryRatio): SmoothDistConf =
    bare(delta, IcdfSamplingConf(samplingSize, samplingBoundaryRatio))

  def bare(delta: Double, sampling: IcdfSamplingConf): SmoothDistConf = SmoothDistConfImpl(delta, sampling)

  def default: SmoothDistConf = DefaultSmoothDistConf

  def forDistConf(distConf: DistConf): SmoothDistConf = apply(delta = distConf.delta)

}

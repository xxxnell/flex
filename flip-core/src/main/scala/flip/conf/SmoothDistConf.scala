package flip.conf

import flip.pdf.SmoothDist
import flip.pdf.sampling.IcdfSampling

trait SmoothDistConfB[D <: SmoothDist[_]] extends DistConfB[D] {

  val sampling: IcdfSamplingConf

}

object DefaultSmoothDistConf extends SmoothDistConf {

  val delta: Double = DefaultSketchConf.delta

  val sampling: IcdfSamplingConf = IcdfSamplingConf.default

}

object SmoothDistConf {

  private case class SmoothDistConfImpl(delta: Double, sampling: IcdfSamplingConf) extends SmoothDistConf

  def apply(delta: Double = DefaultSmoothDistConf.delta,
            sampling: IcdfSamplingConf = DefaultSmoothDistConf.sampling): SmoothDistConf = bare(delta, sampling)

  def bare(delta: Double, sampling: IcdfSamplingConf): SmoothDistConf = SmoothDistConfImpl(delta, sampling)

  def default: SmoothDistConf = DefaultSmoothDistConf

  def forDistConf(distConf: DistConf): SmoothDistConf = apply(delta = distConf.delta)

}

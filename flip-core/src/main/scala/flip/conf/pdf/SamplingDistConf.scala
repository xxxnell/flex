package flip.conf.pdf

import flip.pdf.SamplingDist

trait SamplingDistConfB[+D <: SamplingDist[_]] extends DistConfB[D] {}

object SamplingDistConf {

  private case class SamplingDistConfImpl(delta: Double) extends SamplingDistConf

  def forDistConf(distConf: DistConf): SamplingDistConf = SamplingDistConfImpl(distConf.delta)

}

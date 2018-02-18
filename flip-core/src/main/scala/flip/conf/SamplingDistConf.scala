package flip.conf

trait SamplingDistConf extends DistConf {}

object SamplingDistConf {

  private case class SamplingDistConfImpl(delta: Double) extends SamplingDistConf

  def forDistConf(distConf: DistConf): SamplingDistConf = SamplingDistConfImpl(distConf.delta)

}

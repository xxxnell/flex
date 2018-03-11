package flip.conf

import flip.pdf.SmoothDist

trait SmoothDistConfB[D <: SmoothDist[_]] extends DistConfB[D] {}

object DefaultSmoothDistConf extends SmoothDistConf {

  val delta: Double = DefaultSketchConf.delta

}

object SmoothDistConf {

  private case class SmoothDistConfImpl(delta: Double) extends SmoothDistConf

  def forDistConf(distConf: DistConf): SmoothDistConf = SmoothDistConfImpl(distConf.delta)

  def apply(delta: Double = DefaultSmoothDistConf.delta): SmoothDistConf = bare(delta)

  def bare(delta: Double): SmoothDistConf = SmoothDistConfImpl(delta)

  def default: SmoothDistConf = DefaultSmoothDistConf

}

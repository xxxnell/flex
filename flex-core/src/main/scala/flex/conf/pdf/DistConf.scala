package flex.conf.pdf

import flex.pdf.Dist

trait DistConfB[+D <: Dist[_]] {

  val delta: Double

}

object DistConf {

  private case class DistConfImpl(delta: Double) extends DistConf

  def apply(delta: Double): DistConf = bare(delta)

  def bare(delta: Double): DistConf = DistConfImpl(delta)

}

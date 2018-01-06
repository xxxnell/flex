package sketch.scope.conf

trait DistConf {

  val delta: Double

}

object DistConf {

  private case class DistConfImpl(delta: Double) extends DistConf

  def apply(delta: Double): DistConf = bare(delta)

  def bare(delta: Double): DistConf = DistConfImpl(delta)

}
package sketch.scope.conf

trait DistConf {

}

object DistConf {

  private case class DistConfImpl() extends DistConf

  def apply(): DistConf = bare()

  def bare(): DistConf = DistConfImpl()

}
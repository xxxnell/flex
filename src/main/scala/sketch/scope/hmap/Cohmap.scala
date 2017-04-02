package sketch.scope.hmap

import sketch.scope.counter.CDim

/**
  * Licensed by Probe Technology, Inc.
  *
  * Contravariant of the HMap.
  */
trait Cohmap {

  /**
    * @return
    * */
  def apply(cdim: CDim): Option[HDim]

}

trait CohmapOps {

  def kleisli(cohmap: Cohmap) = ???

}

object Cohmap extends CohmapOps {

  def apply: Cohmap = ???

}

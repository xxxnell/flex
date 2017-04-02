package sketch.scope.hmap

import cats.data.Kleisli
import sketch.scope.Result
import sketch.scope.counter.CDim

/**
  * Licensed by Probe Technology, Inc.
  *
  * Hashing Map.
  */
trait Hmap {

  /**
    * @return cdim
    * */
  def apply(hdim: HDim): Option[CDim]

}

trait HmapOps {

  def kleisli(hmap: Hmap) = Kleisli[Option, Int, Int](hdim => hmap.apply(hdim))

}

object Hmap extends HmapOps {

  def apply: Hmap = ???

}

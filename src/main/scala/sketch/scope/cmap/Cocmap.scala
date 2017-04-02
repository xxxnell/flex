package sketch.scope.cmap

import cats.data.Kleisli
import sketch.scope.counter.CDim

/**
  * Licensed by Probe Technology, Inc.
  *
  * Contravariant of the Cmap.
  */
trait Cocmap[A] {

  def apply(cdim: CDim): A

}

trait CocmapOps {

  def kleisli[A](cocmap: Cocmap[A]) = ???

}

object Cocmap extends CocmapOps {

  def apply[A]: Cocmap[A] = ???

}

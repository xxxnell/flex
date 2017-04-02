package sketch.scope.cmap

import cats.data.Kleisli
import sketch.scope.hmap.HDim

/**
  * Licensed by Probe Technology, Inc.
  *
  * Characteristic Map of Sketch Algorithm.
  */
trait Cmap[A] {

  /**
    * @return hdim
    * */
  def apply(a: A): HDim

}

trait CmapOp {

  def kleisli[A](cmap: Cmap[A]) = Kleisli[Option, A, HDim](a => Some(cmap.apply(a)))

}

object Cmap {

  def apply[A]: Cmap[A] = ???

  def uniform[A]: Cmap[A] = ???

}

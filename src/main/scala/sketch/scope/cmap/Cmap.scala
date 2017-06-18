package sketch.scope.cmap

import cats.data.Kleisli
import sketch.scope.hmap.HDim

/**
  * Licensed by Probe Technology, Inc.
  *
  * Characteristic Map of Sketch Algorithm.
  */
trait Cmap {

  /**
    * @return hdim
    * */
  def apply(a: Double): HDim

}

trait CmapOps {

  def kleisli(cmap: Cmap) = Kleisli[Option, Double, HDim](a => Some(cmap.apply(a)))

}

object Cmap extends CmapOps {

  def uniform(n: Int): Cmap = UniformCmap(n)

  def divider(divider: List[Double]): Cmap = DividerCmap(divider)

}

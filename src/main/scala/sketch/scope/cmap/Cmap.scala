package sketch.scope.cmap

import cats.data.Kleisli
import sketch.scope.hmap.HDim

import scala.collection.immutable.NumericRange

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

trait CmapOps[C<:Cmap] extends CmapLaws[C] {

  def bin(cmap: C): List[NumericRange[Double]]

  def size(cmap: C): Int

  def range(cmap: C, hdim: HDim): NumericRange[Double]

}

trait CmapLaws[C<:Cmap] { self: CmapOps[C] =>

  def kleisli(cmap: Cmap) = Kleisli[Option, Double, HDim](a => Some(cmap.apply(a)))

}

trait CmapSyntax {

  implicit class CmapSyntaxImpl(cmap: Cmap) {
    def bin: List[NumericRange[Double]] = Cmap.bin(cmap)
    def size: Int = Cmap.size(cmap)
    def range(hdim: HDim): NumericRange[Double] = Cmap.range(cmap, hdim)
  }

}

object Cmap extends CmapOps[Cmap] {

  def uniform(n: Int): Cmap = UniformCmap(n)

  def divider(divider: List[Double]): Cmap = DividerCmap(divider)

  def bin(cmap: Cmap): List[NumericRange[Double]] = ???

  def size(cmap: Cmap): Int = ???

  def range(cmap: Cmap, hdim: HDim): NumericRange[Double] = ???

}

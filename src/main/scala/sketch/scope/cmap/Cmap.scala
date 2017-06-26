package sketch.scope.cmap

import cats.data.Kleisli
import sketch.scope.hmap.HDim
import sketch.scope.sketch._

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

  def bin(cmap: C): List[Range]

  def size(cmap: C): Int

  def range(cmap: C, hdim: HDim): Range

}

trait CmapLaws[C<:Cmap] { self: CmapOps[C] =>

  def kleisli(cmap: Cmap) = Kleisli[Option, Double, HDim](a => Some(cmap.apply(a)))

}

trait CmapSyntax {

  implicit class CmapSyntaxImpl(cmap: Cmap) {
    def bin: List[Range] = Cmap.bin(cmap)
    def size: Int = Cmap.size(cmap)
    def range(hdim: HDim): Range = Cmap.range(cmap, hdim)
  }

}

object Cmap extends CmapOps[Cmap] {

  def uniform(n: Int): Cmap = UniformCmap(n)

  def divider(divider: List[Double]): Cmap = DividerCmap(divider)

  def bin(cmap: Cmap): List[Range] = cmap match {
    case cmap: DividerCmap => DividerCmap.bin(cmap)
  }

  def size(cmap: Cmap): Int = cmap match {
    case cmap: DividerCmap => DividerCmap.size(cmap)
  }

  def range(cmap: Cmap, hdim: HDim): Range = cmap match {
    case cmap: DividerCmap => DividerCmap.range(cmap, hdim)
  }

}

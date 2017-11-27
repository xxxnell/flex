package sketch.scope.cmap

import cats.data.Kleisli
import sketch.scope.hmap.HDim
import sketch.scope.dist._

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

  def kleisli(cmap: Cmap): Kleisli[Option, Prim, HDim] = Kleisli[Option, Double, HDim](a => Some(cmap.apply(a)))

  def ranges(cmap: C): List[(HDim, Range)] = (for { i <- 0 until cmap.size } yield (i, range(cmap, i))).toList

}

trait CmapSyntax {

  implicit class CmapSyntaxImpl(cmap: Cmap) {
    def bin: List[Range] = Cmap.bin(cmap)
    def size: Int = Cmap.size(cmap)
    def range(hdim: HDim): Range = Cmap.range(cmap, hdim)
    def ranges: List[(HDim, Range)] = Cmap.ranges(cmap)
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

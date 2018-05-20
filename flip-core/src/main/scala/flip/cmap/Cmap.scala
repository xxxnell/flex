package flip.cmap

import cats.data.Kleisli
import flip.conf.cmap.{CmapConf, UniformCmapConf}
import flip.hmap.HDim
import flip.pdf._
import flip.range.syntax._

/**
  * Cmap, or Characteristic Map is a component of the Sketch algorithm. This is
  * the projection from the primitive value of Sketch to the index of the Hmap.
  */
trait Cmap {

  /**
    * @return Prim => HDim. HDim starts from 0.
    * */
  def apply(a: Prim): HDim

}

trait CmapOps[C <: Cmap] extends CmapLaws[C] {

  val min: Prim = Double.MinValue

  val max: Prim = Double.MaxValue

  def bin(cmap: C): List[RangeP]

  def size(cmap: C): Int

  def range(cmap: C, hdim: HDim): RangeP

}

trait CmapLaws[C <: Cmap] { self: CmapOps[C] =>

  def kleisli(cmap: Cmap): Kleisli[Option, Prim, HDim] = Kleisli[Option, Double, HDim](a => Some(cmap.apply(a)))

  def ranges(cmap: C): List[(HDim, RangeP)] = (for { i <- 0 until cmap.size } yield (i, range(cmap, i))).toList

  def headRange(cmap: C): RangeP = range(cmap, 0)

  def lastRange(cmap: C): RangeP = {
    val size = self.size(cmap)

    range(cmap, if (size > 0) size - 1 else 0)
  }

}

trait CmapSyntax {

  implicit class CmapSyntaxImpl(cmap: Cmap) {
    def bin: List[RangeP] = Cmap.bin(cmap)
    def size: Int = Cmap.size(cmap)
    def range(hdim: HDim): RangeP = Cmap.range(cmap, hdim)
    def ranges: List[(HDim, RangeP)] = Cmap.ranges(cmap)
    def headRange: RangeP = Cmap.headRange(cmap)
    def lastRange: RangeP = Cmap.lastRange(cmap)
  }

}

object Cmap extends CmapOps[Cmap] {

  def apply(conf: CmapConf): Cmap = conf match {
    case conf: UniformCmapConf => Cmap.uniform(conf.size, conf.start, conf.end)
    case _ => ???
  }

  def uniform(n: Int, start: Option[Prim] = None, end: Option[Prim] = None): UniformCmap = UniformCmap(n, start, end)

  def divider(divider: List[Prim]): DividerCmap = DividerCmap(divider)

  def bin(cmap: Cmap): List[RangeP] = cmap match {
    case cmap: DividerCmap => DividerCmap.bin(cmap)
  }

  def size(cmap: Cmap): Int = cmap match {
    case cmap: DividerCmap => DividerCmap.size(cmap)
  }

  def range(cmap: Cmap, hdim: HDim): RangeP = cmap match {
    case cmap: DividerCmap => DividerCmap.range(cmap, hdim)
  }

}

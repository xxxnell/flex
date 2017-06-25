package sketch.scope.cmap

import sketch.scope.hmap.HDim

import scala.collection.immutable.{NumericRange, TreeMap}

/**
  * Licensed by Probe Technology, Inc.
  */
trait DividerCmap extends Cmap {

  val divider: List[Double]

  lazy val index: TreeMap[Double, HDim] = DividerCmap.divider2IndexingMap(divider)

  def apply(a: Double): HDim = index.from(a).headOption.fold(0){ case (_, idx) => idx + 1 }

}

trait DividerCmapOps[DC<:DividerCmap] extends CmapOps[DC] {

  def divider2IndexingMap(divider: List[Double]): TreeMap[Double, HDim] =
    TreeMap.apply(divider.sorted.zipWithIndex: _*)

}

object DividerCmap extends DividerCmapOps[DividerCmap] {

  private case class DividerCmapImpl(divider: List[Double]) extends DividerCmap

  def apply(divider: List[Double]): DividerCmap = DividerCmapImpl(divider)

  def bin(cmap: DividerCmap): List[NumericRange[Double]] = ???

  def size(cmap: DividerCmap): Int = ???

}

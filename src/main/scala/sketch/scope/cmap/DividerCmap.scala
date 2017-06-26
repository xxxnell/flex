package sketch.scope.cmap

import sketch.scope.hmap.HDim
import sketch.scope.sketch._

import scala.collection.immutable.TreeMap

/**
  * Licensed by Probe Technology, Inc.
  */
trait DividerCmap extends Cmap {

  val divider: List[Double]

  lazy val index: TreeMap[Double, HDim] = DividerCmap.divider2IndexingMap(divider)

  lazy val inverseIndex: TreeMap[HDim, Double] = DividerCmap.divider2InverseIndexingMap(divider)

  def apply(a: Double): HDim = index.from(a).headOption.fold(0){ case (_, idx) => idx + 1 }

}

trait DividerCmapOps[DC<:DividerCmap] extends CmapOps[DC] {

  def divider2IndexingMap(divider: List[Double]): TreeMap[Double, HDim] =
    TreeMap.apply(divider.sorted.zipWithIndex: _*)

  def divider2InverseIndexingMap(divider: List[Double]): TreeMap[HDim, Double] =
    TreeMap.apply(divider.sorted.zipWithIndex.map { case (div, idx) => (idx, div) }: _*)

  val min = Double.MinValue

  val max = Double.MaxValue

  def bin(cmap: DC): List[Range] = {
    val divider = cmap.divider
    (min :: divider).zip(divider :+ max).map { case (from, to) => (from to to).by(1) }
  }

  def size(cmap: DC): Int = {
    cmap.divider.size + 1
  }

  def range(cmap: DC, hdim: HDim): Range = {
    val from = cmap.inverseIndex.get(hdim - 1).fold(min)(identity)
    val to = cmap.inverseIndex.get(hdim).fold(max)(identity)
    (from to to).by(1)
  }

}

object DividerCmap extends DividerCmapOps[DividerCmap] {

  private case class DividerCmapImpl(divider: List[Double]) extends DividerCmap

  def apply(divider: List[Double]): DividerCmap = DividerCmapImpl(divider)

}

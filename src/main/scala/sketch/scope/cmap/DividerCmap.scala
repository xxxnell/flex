package sketch.scope.cmap

import sketch.scope.hmap.HDim

import scala.collection.immutable.TreeMap

/**
  * Licensed by Probe Technology, Inc.
  */
trait DividerCmap extends Cmap {

  val divider: List[Double]

  lazy val index: TreeMap[Double, HDim] = DividerCmap.divider2IndexingMap(divider)

  def apply(a: Double): HDim = index.from(a).headOption.fold(0){ case (_, idx) => idx + 1 }

}

trait DividerCmapOps extends CmapOps {

  def divider2IndexingMap(divider: List[Double]): TreeMap[Double, HDim] =
    TreeMap.apply(divider.sorted.zipWithIndex: _*)

}

object DividerCmap extends DividerCmapOps {

  private case class DividerCmapImpl(divider: List[Double]) extends DividerCmap

  def apply(divider: List[Double]): DividerCmap = DividerCmapImpl(divider)

}

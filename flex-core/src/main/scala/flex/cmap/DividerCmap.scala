package flex.cmap

import flex.hmap.HDim
import flex.pdf._
import flex.range.RangeP
import flex.range.syntax._

import scala.collection.immutable.TreeMap

trait DividerCmap extends Cmap {

  val divider: List[Prim]

  lazy val index: TreeMap[Prim, HDim] = DividerCmap.divider2IndexingMap(divider)

  lazy val inverseIndex: TreeMap[HDim, Prim] = DividerCmap.divider2InverseIndexingMap(divider)

  /**
   * @return [x_1, x_2)
   * */
  def apply(a: Double): HDim = index.to(a).lastOption.fold(0) { case (_, idx) => idx + 1 }

  override def equals(other: Any): Boolean =
    other.isInstanceOf[DividerCmap] &&
    (divider == other.asInstanceOf[DividerCmap].divider)

  override def toString: String = {
    def divider2Str(divider: List[Prim]): String =
      ((DividerCmap.min :: divider.flatMap(a => List(a, a))) :+ DividerCmap.max)
        .grouped(2)
        .flatMap {
          case a1 :: a2 :: _ => Some(RangeP(a1, a2))
          case _ => None
        }
        .mkString(", ")

    val sizeLimit = 100
    val dividerStr =
      if (divider.size <= sizeLimit) divider2Str(divider)
      else
        s"${divider2Str(divider.take(sizeLimit / 2))}, ......, ${divider2Str(divider.takeRight(sizeLimit / 2))}"

    s"DividerCmap($dividerStr)"
  }

}

trait DividerCmapOps[DC <: DividerCmap] extends CmapOps[DC] {

  def divider2IndexingMap(divider: List[Double]): TreeMap[Double, HDim] =
    TreeMap.apply(divider.sorted.zipWithIndex: _*)

  def divider2InverseIndexingMap(divider: List[Double]): TreeMap[HDim, Double] =
    TreeMap.apply(divider.sorted.zipWithIndex.map { case (div, idx) => (idx, div) }: _*)

  def bins(cmap: DC): List[RangeP] = {
    val divider = cmap.divider

    (min :: divider).zip(divider :+ max).map { case (start, end) => RangeP(start, end) }
  }

  def binsArr(cmap: DC): Array[RangeP] = {
    val divider = cmap.divider.toArray
    var i = 1
    val bins = Array.ofDim[RangeP](divider.length + 1)
    var start = divider(0)
    bins.update(0, RangeP(min, start))
    while (i < divider.length) {
      val end = divider(i)
      bins.update(i, RangeP(start, end))
      start = end
      i += 1
    }
    bins.update(divider.length, RangeP(start, max))
    bins
  }

  def size(cmap: DC): Int =
    cmap.divider.size + 1

  def range(cmap: DC, hdim: HDim): RangeP = {
    val start = cmap.inverseIndex.getOrElse(hdim - 1, min)
    val end = cmap.inverseIndex.getOrElse(hdim, max)

    RangeP(start, end)
  }

}

object DividerCmap extends DividerCmapOps[DividerCmap] {

  private case class DividerCmapImpl(divider: List[Double]) extends DividerCmap

  def apply(divider: List[Double]): DividerCmap = DividerCmapImpl(divider)

}

package flex.pdf.syntax

import flex.hcounter.HCounter
import flex.pdf.{Count, DataBinningDist, DataBinningDistOps, Histogram, HistogramOps}
import flex.plot.DensityPlot

import scala.language.higherKinds

trait DataBinningDistSyntax extends DataBinningDistPropSyntax with HistogramPropSyntax

trait DataBinningDistPropPolySyntaxImpl[A, D[_] <: DataBinningDist[_], Op <: DataBinningDistOps[D]] {
  // context
  def dist: D[A]
  def op: Op
  // syntax
  def update(as: A*): D[A] =
    op.update(dist, as.toList.map(a => (a, 1.0)))
  def update(as: List[(A, Count)]): D[A] =
    op.update(dist, as)
  def updateInOrder(as: List[A]): D[A] =
    as.foldLeft(dist) { case (_dist, a) => op.update(_dist, (a, 1.0) :: Nil) }
  def updateTrace(as: List[A]): List[D[A]] = {
    var temp: D[A] = dist
    as.map { a =>
      temp = op.update(temp, (a, 1.0) :: Nil)
      temp
    }
  }
}

trait DataBinningDistPropSyntax {

  implicit class DataBinningDistPropSyntaxImpl[A](dbdist: DataBinningDist[A])
      extends DataBinningDistPropPolySyntaxImpl[A, DataBinningDist, DataBinningDistOps[DataBinningDist]] {
    // context
    def dist: DataBinningDist[A] = dbdist
    def op: DataBinningDistOps[DataBinningDist] = DataBinningDist
    // syntax
    def count(start: A, end: A): Count = op.count(dist, start, end)
    def sum: Count = op.sum(dist)
  }

}

trait HistogramPropSyntax {

  implicit class HistogramPropSyntaxImpl[A](hist: Histogram[A])
      extends DataBinningDistPropPolySyntaxImpl[A, Histogram, HistogramOps[Histogram]] {
    // context
    def dist: Histogram[A] = hist
    def op: HistogramOps[Histogram] = Histogram
    // syntax
    def modifyCounter(f: HCounter => HCounter): Histogram[A] = Histogram.modifyCounter(hist, f)
    def barPlot: DensityPlot = Histogram.rangeSampling(hist)
    def scanUpdate(as: List[(A, Count)]): Histogram[A] = Histogram.scanUpdate(hist, as)
  }

}

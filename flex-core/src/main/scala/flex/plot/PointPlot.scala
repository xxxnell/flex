package flex.plot

import flex._
import flex.pdf.{Count, Prim}
import flex.range.RangeP
import cats.data.NonEmptyList

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

trait PointPlot extends Plot {

  def records: Array[(Double, Double)]

  lazy val index: TreeMap[Double, Int] = {
    var i = 0
    val arr = Array.ofDim[(Double, Int)](records.length)
    while (i < records.length) {
      val (x, _) = records.apply(i)
      arr.update(i, (x, i))
      i += 1
    }
    TreeMap(arr: _*)
  }

  override def toString: String = "PointPlot(" + records.map { case (x, y) => s"$x -> $y" }.mkString(", ") + ")"

}

trait PointPlotOps[P <: PointPlot] extends PlotOps[P] with PointPlotLaws[P] {

  def modifyRecords(plot: P, f: Array[(Double, Double)] => Array[(Double, Double)]): P

}

trait PointPlotLaws[P <: PointPlot] { self: PointPlotOps[P] =>

  def map(plot: P, f: (Double, Double) => (Double, Double)): P =
    modifyRecords(
      plot,
      records0 => {
        var i = 0
        val records1 = Array.ofDim[(Double, Double)](records0.length)
        while (i < records0.length) {
          val (x, y) = records0.apply(i)
          records1.update(i, f(x, y))
          i += 1
        }
        records1
      }
    )

  def interpolation(plot: P, x: Double): Double = {
    val records = plot.records

    lazy val intp1 = plot.index.from(x).headOption.flatMap {
      case (_, i2) =>
        if (i2 > 0) {
          val (x1, y1) = records(i2 - 1)
          val (x2, y2) = records(i2)
          Fitting((x1, y1) :: (x2, y2) :: Nil, x)
        } else Some(records(i2)._2)
    }
    lazy val intp2 = plot.index.to(x).lastOption.flatMap {
      case (_, i2) =>
        if (i2 + 1 < records.length) {
          val (x1, y1) = records(i2)
          val (x2, y2) = records(i2 + 1)
          Fitting((x1, y1) :: (x2, y2) :: Nil, x)
        } else Some(records(i2)._2)
    }
    lazy val intp3 = if (plot.records.length > 0) {
      val (x2, y2) = plot.records.apply(0)
      val (x3, y3) = plot.records.apply(plot.records.length - 1)
      if (x <= x2) Some(y2) else if (x >= x3) Some(y3) else None
    } else None

    (intp1 orElse intp2 orElse intp3).getOrElse(Double.NaN)
  }

  /**
    * @param i Referencial index for the records of the plot.
    * */
  def referencialInterpolation(plot: P, x: Double, i: Int): Option[Double] = {
    val records = plot.records
    def refsForShift(j: Int): Option[((Double, Double), (Double, Double))] =
      if (i + j < records.length && i + j - 1 >= 0) {
        Some((records.apply(i + j - 1), records.apply(i + j)))
      } else if (i + j - 1 < records.length && i + j - 2 >= 0) {
        Some((records.apply(i + j - 2), records.apply(i + j - 1)))
      } else None

    (refsForShift(1) orElse refsForShift(0))
      .flatMap { case (ref1, ref2) => if (ref1._1 <= x && ref2._1 >= x) Fitting(ref1 :: ref2 :: Nil, x) else None }
  }

  def add(plots: NonEmptyList[(Double, P)]): P =
    modifyRecords(
      plots.head._2,
      _ => {
        val _plots: Array[(Double, P)] = plots.toList.toArray
        val size = plots.toList.map { case (_, plot) => plot.records.length }.sum

        val records1 = Array.ofDim[(Double, Double)](size)
        val idxs = Array.fill(_plots.length)(0)
        var i = 0

        while (i < size) {
          var j = 0
          var xMin = Double.MaxValue
          var minIdx = 0
          while (j < _plots.length) {
            val (_, plot) = _plots.apply(j)
            val x = if (idxs(j) < plot.records.length) plot.records.apply(idxs(j))._1 else Double.MaxValue
            if (x < xMin) {
              xMin = x
              minIdx = j
            }
            j += 1
          }

          var k = 0
          var y2 = 0.0
          while (k < _plots.length) {
            val (w, _plot) = _plots.apply(k)
            val idx = idxs(k)
            val ref = if (idx < _plot.records.length) idx else _plot.records.length - 1
            y2 += w * referencialInterpolation(_plot, xMin, ref).getOrElse(interpolation(_plot, xMin))
            k += 1
          }

          records1.update(i, (xMin, y2))
          idxs.update(minIdx, idxs(minIdx) + 1)
          i += 1
        }

        records1
      }
    )

  def inverse(plot: P): P =
    modifyRecords(
      plot,
      (records0: Array[(Double, Double)]) => {
        var i = 0
        val records1 = Array.ofDim[(Double, Double)](records0.length)
        while (i < records0.length) {
          val (x, y) = records0(i)
          records1.update(i, (y, x))
          i += 1
        }
        records1
      }
    )

  def normalizedCumulative(plot: P): P =
    modifyRecords(
      plot,
      (records0: Array[(Double, Double)]) => {
        var (i, cum) = (0, 0.0)
        var (x1, y1) = (Double.NaN, Double.NaN)
        val sum = integralAll(plot)
        val length = records0.length
        val records1 = Array.ofDim[(Double, Double)](length)
        while (i < records0.length) {
          val (x2, y2) = records0(i)
          cum += (if (!x1.isNaN && !y1.isNaN) areaPoint(x1, y1, x2, y2) else 0.0) / sum
          records1.update(i, (x2, cum))
          x1 = x2
          y1 = y2
          i += 1
        }
        records1
      }
    )

  def inverseNormalizedCumulative(plot: P): P =
    inverse(normalizedCumulative(plot))

  def changeRate(plot: P): P =
    modifyRecords(
      plot,
      records0 => {
        var i = 0
        val records1 = new ArrayBuffer[(Double, Double)]
        while (i < records0.length) {
          lazy val (x0, y0) = records0.apply(i - 1)
          lazy val (x1, y1) = records0.apply(i)
          if (i > 0 && x0 != x1) records1.append((x0 / 2 + x1 / 2, (y1 - y0) / (x1 - x0)))
          i += 1
        }
        records1.toArray
      }
    )

  def integralAll(plot: P): Double = {
    val records = plot.records
    var acc = 0.0
    var (x1, y1) = (Double.NaN, Double.NaN)
    var i = 0
    while (i < records.length) {
      val (x2, y2) = records.apply(i)
      acc += (if (!x1.isNaN && !y1.isNaN) areaPoint(x1, y1, x2, y2) else 0.0)
      x1 = x2
      y1 = y2
      i += 1
    }
    acc
  }

  def areaPoint(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    if (y1 == 0 && y2 == 0) 0
    else RangeP(x1, x2).roughLength * (y2 / 2 + y1 / 2)
  }

  def isEmpty(plot: P): Boolean = plot.records.isEmpty

  def domain(plot: P): RangeP = RangeP.apply(plot.records.last._1, plot.records.head._1)

}

object PointPlot extends PointPlotOps[PointPlot] {

  private case class PointPlotImpl(records: Array[(Double, Double)]) extends PointPlot

  def apply(records: Array[(Double, Double)]): PointPlot = safe(records)

  def unsafe(records: Array[(Double, Double)]): PointPlot = PointPlotImpl(records)

  def safe(records: Array[(Double, Double)]): PointPlot = unsafe(records.sortBy(_._1))

  def empty: PointPlot = unsafe(Array.empty[(Double, Double)])

  def deltas(ds: List[(Prim, Count)], window: Double): PointPlot = {
    val sum = ds.map(d => d._2).sum
    val _window = if (window <= 0) 1e-100 else window
    val dsArr1 = ds.sortBy(_._1).toArray

    // merge
    var i = 0
    val diff = if (dsArr1.length > 0) Array.ofDim[Double](dsArr1.length - 1) else Array.empty[Double]
    while (i < dsArr1.length - 1) {
      diff.update(i, dsArr1.apply(i + 1)._1 - dsArr1.apply(i)._1)
      i += 1
    }
    var j = 0
    val dsArr2 = new ArrayBuffer[(Double, Double)]
    while (j < dsArr1.length) {
      val _diff = if (j > 0) diff.apply(j - 1) else Double.MaxValue
      val (x1, count1) = dsArr1.apply(j)
      lazy val (x0, count0) = dsArr2.apply(dsArr2.length - 1)
      if (_diff > window * 2) dsArr2.append((x1, count1))
      else dsArr2.update(dsArr2.length - 1, (x0, count0 + count1))
      j += 1
    }

    // deltas
    var k = 0
    val records = Array.ofDim[(Double, Double)](dsArr2.length * 3)
    while (k < dsArr2.length) {
      val (value, count) = dsArr2.apply(k)
      val x1 = value - (_window / 2)
      val x2 = value
      val x3 = value + (_window / 2)
      val y = if (sum * _window > 0) (count * 2) / _window else 0
      records.update(k * 3, (x1, 0))
      records.update(k * 3 + 1, (x2, y))
      records.update(k * 3 + 2, (x3, 0))
      k += 1
    }

    unsafe(records)
  }

  def unsafeCumulative(ds: List[(Prim, Count)]): PointPlot = {
    val records = ds.toArray
    var i = 0
    var cum = 0.0
    val records1 = Array.ofDim[(Prim, Count)](records.length)
    while (i < records.length) {
      val (p, count) = records.apply(i)
      cum += count
      records1.update(i, (p, cum))
      i += 1
    }
    unsafe(records1)
  }

  def cumulative(ds: List[(Prim, Count)]): PointPlot = {
    unsafeCumulative(ds.sortBy(_._1))
  }

  def unsafeNormalizedCumulative(ds: List[(Prim, Count)]): PointPlot = {
    val records = ds.toArray
    var i = 0
    var sum = 0.0
    while (i < records.length) {
      sum += records.apply(i)._2
      i += 1
    }
    var j = 0
    var cum = 0.0
    val records1 = Array.ofDim[(Prim, Count)](records.length)
    while (j < records.length) {
      val (p, count) = records.apply(j)
      cum += (count / sum)
      records1.update(j, (p, cum))
      j += 1
    }
    unsafe(records1)
  }

  def normalizedCumulative(ds: List[(Prim, Count)]): PointPlot = {
    unsafeNormalizedCumulative(ds.sortBy(_._1))
  }

  def modifyRecords(plot: PointPlot, f: Array[(Double, Double)] => Array[(Double, Double)]): PointPlot =
    unsafe(f(plot.records))

}

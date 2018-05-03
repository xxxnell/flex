package flip.plot

import cats.data.NonEmptyList

import scala.collection.immutable.{TreeMap, TreeSet}

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

  def interpolation(plot: P, x: Double): Double = {
    val records = plot.records
    plot.index.from(x).headOption.fold(0.0) {
      case (x2, i2) =>
        if (i2 > 0) {
          val (x1, y1) = records(i2 - 1)
          val (x2, y2) = records(i2)
          Fitting((x1, y1) :: (x2, y2) :: Nil, x).getOrElse(0.0)
        } else 0.0
    }
  }

  /**
    * @param i Referencial index for the records of the plot.
    * */
  def referencialInterpolation(plot: P, x: Double, i: Int): Double = {
    val records = plot.records
    def refs(j: Int): Option[List[(Double, Double)]] =
      if (i + j < records.length && i + j - 1 >= 0) {
        Some(records.apply(i + j - 1) :: records.apply(i + j) :: Nil)
      } else if (i + j - 1 < records.length && i + j - 2 >= 0) {
        Some(records.apply(i + j - 2) :: records.apply(i + j - 1) :: Nil)
      } else None

    (refs(1).flatMap(refs1 => Fitting(refs1, x)) orElse
      refs(0).flatMap(refs0 => Fitting(refs0, x)))
      .getOrElse(interpolation(plot, x))
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
            y2 += w * referencialInterpolation(_plot, xMin, ref)
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
        var (i, j, sum, cum) = (0, 0, 0.0, 0.0)
        val records1 = Array.ofDim[(Double, Double)](records0.length)
        while (i < records0.length) {
          val (_, y) = records0(i)
          sum += y
          i += 1
        }
        while (j < records0.length) {
          val (x, y) = records0(j)
          cum += y / sum
          records1.update(j, (x, cum))
          j += 1
        }
        records1
      }
    )

  def inverseNormalizedCumulative(plot: P): P =
    modifyRecords(
      plot,
      (records0: Array[(Double, Double)]) => {
        var (i, j, sum, cum) = (0, 0, 0.0, 0.0)
        val records1 = Array.ofDim[(Double, Double)](records0.length)
        while (i < records0.length) {
          val (_, y) = records0(i)
          sum += y
          i += 1
        }
        while (j < records0.length) {
          val (x, y) = records0(j)
          cum += y / sum
          records1.update(j, (cum, x))
          j += 1
        }
        records1
      }
    )

}

object PointPlot extends PointPlotOps[PointPlot] {

  private case class PointPlotImpl(records: Array[(Double, Double)]) extends PointPlot

  def apply(records: Array[(Double, Double)]): PointPlot = bare(records)

  def bare(records: Array[(Double, Double)]): PointPlot = PointPlotImpl(records)

  def modifyRecords(plot: PointPlot, f: Array[(Double, Double)] => Array[(Double, Double)]): PointPlot =
    bare(f(plot.records))

}

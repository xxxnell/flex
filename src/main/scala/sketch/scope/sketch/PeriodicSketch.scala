package sketch.scope.sketch

import scala.reflect.runtime.universe._
import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter

import scala.collection.immutable.NumericRange
import scala.util.Try
import cats._
import cats.data._
import cats.implicits._
import sketch.scope.hmap.HDim

/**
  * Licensed by Probe Technology, Inc.
  *
  *
  */
trait PeriodicSketch extends Sketch {

  def periods: Stream[Double]

}

trait PeriodicSketchOps[S<:PeriodicSketch] extends SketchOps[S] { self =>

  def bareApply(structure: List[(Cmap, HCounter)], period: Stream[Double]): S

  def primitiveUpdate(sketch: S, p: Double): Option[S] = {
    val utdStrO = sketch.structure.traverse { case (cmap, hcounter) =>
      hcounter.update(cmap.apply(p), 1).map(hcounter => (cmap, hcounter))
    }
    val utdSketchO = utdStrO.map(str => bareApply(str, sketch.periods))

    for {
      sketch <- utdSketchO
      utdSketch <- sketch.periods.headOption.fold(Option(sketch))(period =>
        if(self.sum(sketch) > period) self.renew(dropPeriod(sketch)) else Option(sketch)
      )
    } yield utdSketch
  }

  def singleCount(cmap: Cmap, hcounter: HCounter, pFrom: Double, pTo: Double): Option[Double] = {
    val (fromHdim, toHdim) = (cmap.apply(pFrom), cmap.apply(pTo))
    val (fromRng, toRng) = (cmap.range(fromHdim), cmap.range(toHdim))

    // mid count
    val midRangeO: Option[(HDim, HDim)] = if((toHdim-1) > (fromHdim+1)) {
      Some((fromHdim + 1, toHdim - 1))
    } else None
    val midCountO = midRangeO.fold(Option(0.0)){ case (from, to) => hcounter.count(from, to) }

    // from count
    val fromDensityO = hcounter.count(fromHdim, fromHdim).map(c => c / (fromRng.end - fromRng.start))
    val fromCountO = fromDensityO.map(density => (fromRng.end - pFrom) * density)

    // to count
    val toDensityO = hcounter.count(toHdim, toHdim).map(c => c / (toRng.end - toRng.start))
    val toCountO = toDensityO.map(density => (pTo - toRng.start) * density)

    for {
      toCount <- toCountO
      fromCount <- fromCountO
      midCount <- midCountO
    } yield toCount + fromCount + midCount
  }

  def primitiveCount(sketch: S, pFrom: Double, pTo: Double): Option[Double] = {
    val countsO = sketch.structure.traverse { case (cmap, hcounter) => singleCount(cmap, hcounter, pFrom, pTo) }
    countsO.map(counts => counts.sum / counts.size)
  }

  def sum(sketch: S): Double = {
    val sums = sketch.structure.map { case (_, hcounter) => hcounter.sum }
    sums.sum / sums.size
  }

  def clear(sketch: S): S = ???

  def dropPeriod(sketch: S): S = bareApply(sketch.structure, sketch.periods.drop(1))

  def renew(sketch: S): Option[S] = for {
    cmapHcounter <- sketch.structure.headOption
    (cmap, hcounter) = cmapHcounter
    cmapHcounters = Try(sketch.structure.tail).toOption.fold(List.empty[(Cmap, HCounter)])(identity)
    utdCmap <- renewCmap(sketch)
    structure = cmapHcounters :+ (utdCmap, HCounter.empty(hcounter.depth, hcounter.width))
  } yield bareApply(structure, sketch.periods)

  def renewCmap(sketch: S): Option[Cmap] = {
    val sum = self.sum(sketch)
    val densityPlot = plot(sketch).map { case (range, count) => (range, count / (sum * (range.end - range.start))) }
    val sizeO = sketch.structure.headOption.map { case (cmap, _) => cmap.size }
    val utdRangesO = sizeO.map(size => updateRanges(size, densityPlot))
    val dividerO = utdRangesO.map(ranges => ranges.map(_.start).drop(1))
    dividerO.map(divider => Cmap.divider(divider))
  }

  /**
    * @return (updated ranges, remaining accumulation)
    * */
  def updateRanges(size: Int, plot: List[(Range, Double)]): Ranges = {
    def updateRangesAcc(size:Int, plot: List[(Range, Double)], acc: Ranges): Ranges = {
      val unit = 1 / size.toDouble
      if(size > 0) {
        val splitted = split(unit, plot)
        val rangeO = for {
          head <- splitted._1.headOption
          last <- splitted._1.lastOption
        } yield head._1.start to last._1.end

        rangeO match {
          case None => acc.reverse
          case Some(range) => updateRangesAcc(size - 1, splitted._2, range.by(1) :: acc)
        }
      } else acc.reverse
    }

    updateRangesAcc(size, plot, Nil)
  }

  def split(area: Double, plot: List[(Range, Double)]): (List[(Range, Double)], List[(Range, Double)]) = {
    def splitAcc(area: Double,
                 plot: List[(Range, Double)],
                 acc: (List[(Range, Double)], List[(Range, Double)])):
    (List[(Range, Double)], List[(Range, Double)]) = {
      plot match {
        case Nil => acc
        case (range, density) :: tail =>
          val segArea = (range.end - range.start) * density
          if (segArea >= area) {
            val mid = (area / density) + range.start
            (acc._1 :+ ((range.start to mid).by(1), density), ((mid to range.end).by(1), density) :: acc._2.drop(1))
          } else splitAcc(area - segArea, tail, (acc._1 :+ (range, density), acc._2.drop(1)))
      }
    }

    splitAcc(area, plot, (Nil, plot))
  }

}

object PeriodicSketch {

  private case class SketchImpl(structure: List[(Cmap, HCounter)],
                                periods: Stream[Double])
    extends PeriodicSketch

  def empty[A](cmapSize: Int, depth: Int, cdimSize: Int): Sketch = ???

}

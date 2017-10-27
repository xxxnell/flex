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
trait PeriodicSketch[A] extends Sketch[A] {

  def periods: Stream[Double]

}

trait PeriodicSketchOps[S[_]<:PeriodicSketch[_]] extends SketchOps[S] { self =>

  def bareApply[A](measure: A => Double, structure: Structure, period: Stream[Double]): S[A]

  def primUpdate[A](sketch: S[A], p: Double): Option[S[A]] = {
    val utdStrO = sketch.structure.traverse { case (cmap, hcounter) =>
      hcounter.update(cmap.apply(p), 1).map(hcounter => (cmap, hcounter))
    }
    val utdSketchO = utdStrO.map(str => bareApply(sketch.measure.asInstanceOf[A => Double], str, sketch.periods))

    for {
      sketch <- utdSketchO
      utdSketch <- sketch.periods.headOption.fold(Option(sketch))(period =>
        if(self.sum(sketch) > period) self.rearrange(dropPeriod(sketch)) else Option(sketch)
      )
    } yield utdSketch
  }

//  def clear(sketch: S): S = ???

  def dropPeriod[A](sketch: S[A]): S[A] =
    bareApply(
      sketch.measure.asInstanceOf[A => Double],
      sketch.structure,
      sketch.periods.drop(1)
    )

  def rearrange[A](sketch: S[A]): Option[S[A]] = for {
    cmapHcounter <- sketch.structure.headOption
    (cmap, hcounter) = cmapHcounter
    cmapHcounters = Try(sketch.structure.tail).toOption.fold(List.empty[(Cmap, HCounter)])(identity)
    utdCmap <- rearrangeCmap(sketch)
    structure = cmapHcounters :+ (utdCmap, HCounter.empty(hcounter.depth, hcounter.width))
  } yield bareApply(sketch.measure.asInstanceOf[A => Double], structure, sketch.periods)

  def rearrangeCmap(sketch: S[_]): Option[Cmap] = {
    val densityPlotO = self.densityPlot(sketch)
    val sizeO = sketch.structure.headOption.map { case (cmap, _) => cmap.size }
    val utdRangesO = for {
      densityPlot <- densityPlotO
      size <- sizeO
    } yield updateRanges(size, densityPlot)
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

object PeriodicSketch extends PeriodicSketchOps[PeriodicSketch] {

  private case class PeriodicSketchImpl[A](measure: A => Double,
                                           structure: Structure,
                                           periods: Stream[Double])
    extends PeriodicSketch[A]

  def apply[A](measure: A => Double,
               structure: Structure,
               periods: Stream[Double]): PeriodicSketch[A] =
    bareApply(measure, structure, periods)

  def bareApply[A](measure: A => Double,
                   structure: Structure,
                   periods: Stream[Double]): PeriodicSketch[A] =
    PeriodicSketchImpl(measure, structure, periods)

  def empty[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): PeriodicSketch[A] =
    cont(measure, caDepth, caSize, coDepth, coSize)

  def cont[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): PeriodicSketch[A] = {
    val structure = (1 to caDepth).toList.map(_ => (Cmap.uniform(caSize), HCounter.empty(coDepth, coSize)))
    ContSketch.empty(measure, caDepth, caSize, coDepth, coSize)
  }

}

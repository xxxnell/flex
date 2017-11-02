package sketch.scope.dist

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

trait PeriodicSketchOps[S[_]<:PeriodicSketch[_]] extends SketchPrimPropOps[S] { self =>

  def modifyPeriods[A](sketch: S[A], f: Stream[Double] => Option[Stream[Double]]): Option[S[A]]

  def primUpdate[A](sketch: S[A], p: Double): Option[S[A]] = for {
    sketch <- simpleUpdate(sketch, p)
    utdSketch <- sketch.periods.headOption.fold(Option(sketch))(period =>
      if(self.sum(sketch) > period) dropPeriod(sketch).flatMap(sketch => rearrange(sketch)) else Option(sketch)
    )
  } yield utdSketch

  def dropPeriod[A](sketch: S[A]): Option[S[A]] = modifyPeriods(sketch, periods => Some(periods.drop(1)))

  def rearrange[A](sketch: S[A]): Option[S[A]] = modifyStructure(sketch, structure => for {
    cmapHcounter <- structure.headOption
    (cmap, hcounter) = cmapHcounter
    cmapHcounters = Try(structure.tail).toOption.fold(List.empty[(Cmap, HCounter)])(identity)
    utdCmap <- rearrangeCmap(sketch)
    structure = cmapHcounters :+ (utdCmap, HCounter.empty(hcounter.depth, hcounter.width))
  } yield structure)

  def rearrangeCmap(sketch: S[_]): Option[Cmap] = for {
    densityPlot <- densityPlot(sketch)
    size <- sketch.structure.headOption.map { case (cmap, _) => cmap.size }
    utdRanges = updateRanges(size, densityPlot)
    divider = utdRanges.map(_.start).drop(1)
  } yield Cmap.divider(divider)

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
    bare(measure, structure, periods)

  def bare[A](measure: A => Double,
              structure: Structure,
              periods: Stream[Double]): PeriodicSketch[A] =
    PeriodicSketchImpl(measure, structure, periods)

  def empty[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): PeriodicSketch[A] =
    cont(measure, caDepth, caSize, coDepth, coSize)

  def cont[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): PeriodicSketch[A] =
    ContSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  def modifyStructure[A](sketch: PeriodicSketch[A],
                         f: Structure => Option[Structure]): Option[PeriodicSketch[A]] =
    f(sketch.structure).map(structure => bare(sketch.measure, structure, sketch.periods))

  def modifyPeriods[A](sketch: PeriodicSketch[A],
                       f: Stream[Double] => Option[Stream[Double]]): Option[PeriodicSketch[A]] =
    f(sketch.periods).map(period => bare(sketch.measure, sketch.structure, period))

}

package flip.pdf

import flip.cmap.Cmap
import flip.conf.{AdaPerSketchConf, PeriodicSketchConf, SketchConf}
import flip.hcounter.HCounter
import flip.measure.Measure

import scala.language.higherKinds

/**
  * PeroidicSketch, or Peroidic Sketch provides a concrete way to set
  * thresholds for RecurSketch. It finds infinite thresholds at regular
  * intervals for a given period.
  * */
trait PeriodicSketch[A] extends RecurSketch[A] {

  def conf: PeriodicSketchConf

}

trait PeriodicSketchOps[S[_] <: PeriodicSketch[_]] extends RecurSketchOps[S] {

  def thresholds(start: Double, period: Double): Stream[Count] = Stream.from(0).map(i => start + period * i)

}

object PeriodicSketch extends PeriodicSketchOps[PeriodicSketch] { self =>

  case class PeriodicSketchImpl[A](measure: Measure[A],
                                   conf: PeriodicSketchConf,
                                   structures: Structures,
                                   thresholds: Stream[Count],
                                   count: Count)
      extends PeriodicSketch[A]

  def bare[A](measure: Measure[A],
              conf: PeriodicSketchConf,
              structures: Structures,
              thresholds: Stream[Count],
              count: Count): PeriodicSketch[A] =
    PeriodicSketchImpl(measure, conf, structures, thresholds, count)

  def empty[A](implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] = conf match {
    case conf: AdaPerSketchConf => AdaPerSketch.empty(measure, conf)
    case _ => bare(measure, conf, structures(conf), thresholds(conf.startThreshold, conf.thresholdPeriod), 0)
  }

  def concat[A](as: List[(A, Count)])(implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] =
    conf match {
      case conf: AdaPerSketchConf => AdaPerSketch.concat(as)(measure, conf)
      case _ =>
        val structures = concatStructures(as, measure, conf)
        val thresholds = self.thresholds(conf.startThreshold, conf.thresholdPeriod)
        narrowUpdate(bare(measure, conf, structures, thresholds, 0), as)
    }

  def modifyStructure[A](sketch: PeriodicSketch[A], f: Structures => Structures): PeriodicSketch[A] =
    sketch match {
      case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructure(sketch, f)
      case _ => bare(sketch.measure, sketch.conf, f(sketch.structures), sketch.thresholds, sketch.count)
    }

  def modifyThresholds[A](sketch: PeriodicSketch[A], f: Stream[Double] => Stream[Double]): PeriodicSketch[A] =
    sketch match {
      case sketch: AdaPerSketch[A] => AdaPerSketch.modifyThresholds(sketch, f)
      case _ => bare(sketch.measure, sketch.conf, sketch.structures, f(sketch.thresholds), sketch.count)
    }

  def modifyCount[A](sketch: PeriodicSketch[A], f: Count => Count): PeriodicSketch[A] = sketch match {
    case adaper: AdaPerSketch[A] => AdaPerSketch.modifyCount(adaper, f)
    case _ => bare(sketch.measure, sketch.conf, sketch.structures, sketch.thresholds, f(sketch.count))
  }

  override def update[A](sketch: PeriodicSketch[A], as: List[(A, Count)]): PeriodicSketch[A] = sketch match {
    case (sketch: AdaPerSketch[A]) => AdaPerSketch.update(sketch, as)
    case _ => super.update(sketch, as)
  }

}

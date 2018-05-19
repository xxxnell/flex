package flip.pdf

import flip.conf.{AdaPerSketchConf, PeriodicSketchConf}
import flip.measure.Measure
import flip.rand.IRng

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

  def periodicThresholds(start: Double, period: Double): Stream[Count] = Stream.from(0).map(i => start + period * i)

}

object PeriodicSketch extends PeriodicSketchOps[PeriodicSketch] { self =>

  def empty[A](implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] = conf match {
    case conf: AdaPerSketchConf => AdaPerSketch.empty(measure, conf)
  }

  def concat[A](as: List[(A, Count)])(implicit measure: Measure[A], conf: PeriodicSketchConf): PeriodicSketch[A] =
    conf match {
      case conf: AdaPerSketchConf => AdaPerSketch.concat(as)(measure, conf)
    }

  def modifyRng[A](sketch: PeriodicSketch[A], f: IRng => IRng): PeriodicSketch[A] =
    sketch match {
      case sketch: AdaPerSketch[A] => AdaPerSketch.modifyRng(sketch, f)
    }

  def modifyStructures[A](sketch: PeriodicSketch[A], f: Structures => Structures): PeriodicSketch[A] =
    sketch match {
      case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructures(sketch, f)
    }

  def modifyThresholds[A](sketch: PeriodicSketch[A], f: Stream[Double] => Stream[Double]): PeriodicSketch[A] =
    sketch match {
      case sketch: AdaPerSketch[A] => AdaPerSketch.modifyThresholds(sketch, f)
    }

  def modifyCount[A](sketch: PeriodicSketch[A], f: Count => Count): PeriodicSketch[A] = sketch match {
    case adaper: AdaPerSketch[A] => AdaPerSketch.modifyCount(adaper, f)
  }

  def rebuildCond[A](sketch: PeriodicSketch[A]): Boolean = sketch match {
    case selective: SelectiveSketch[A] => SelectiveSketch.rebuildCond(selective)
    case adaper: AdaPerSketch[A] => AdaPerSketch.rebuildCond(adaper)
  }

  // overrides

  override def update[A](sketch: PeriodicSketch[A], as: List[(A, Count)]): PeriodicSketch[A] = sketch match {
    case sketch: SelectiveSketch[A] => SelectiveSketch.update(sketch, as)
    case sketch: AdaPerSketch[A] => AdaPerSketch.update(sketch, as)
  }

}

package flex.pdf

import flex.conf.pdf.{AdaSelSketchConf, SelectiveSketchConf}
import flex.measure.Measure
import flex.rand.IRng

import scala.language.higherKinds

trait SelectiveSketch[A] extends PeriodicSketch[A] {

  def conf: SelectiveSketchConf

}

trait SelectiveSketchOps[S[_] <: SelectiveSketch[_]] extends PeriodicSketchOps[S] {}

object SelectiveSketch extends SelectiveSketchOps[SelectiveSketch] {

  def empty[A](implicit measure: Measure[A], conf: SelectiveSketchConf): SelectiveSketch[A] = conf match {
    case conf: AdaSelSketchConf => AdaSelSketch.empty(measure, conf)
  }

  def concat[A](as: List[(A, Count)])(implicit measure: Measure[A], conf: SelectiveSketchConf): SelectiveSketch[A] =
    conf match {
      case conf: AdaSelSketchConf => AdaSelSketch.concat(as)(measure, conf)
    }

  override def modifyRng[A](sketch: SelectiveSketch[A], f: IRng => IRng): SelectiveSketch[A] = sketch match {
    case adasel: AdaSelSketch[A] => AdaSelSketch.modifyRng(adasel, f)
  }

  override def modifyStructures[A](sketch: SelectiveSketch[A], f: Structures => Structures): SelectiveSketch[A] =
    sketch match {
      case adasel: AdaSelSketch[A] => AdaSelSketch.modifyStructures(adasel, f)
    }

  override def modifyThresholds[A](sketch: SelectiveSketch[A], f: Stream[Count] => Stream[Count]): SelectiveSketch[A] =
    sketch match {
      case adasel: AdaSelSketch[A] => AdaSelSketch.modifyThresholds(adasel, f)
    }

  override def modifyCount[A](sketch: SelectiveSketch[A], f: Count => Count): SelectiveSketch[A] = sketch match {
    case adasel: AdaSelSketch[A] => AdaSelSketch.modifyCount(adasel, f)
  }

  override def diagnose[A](sketch: SelectiveSketch[A]): Boolean = sketch match {
    case adasel: AdaSelSketch[A] => AdaSelSketch.diagnose(adasel)
  }

  // overrides

  override def update[A](sketch: SelectiveSketch[A], as: List[(A, Count)]): SelectiveSketch[A] = sketch match {
    case (sketch: AdaSelSketch[A]) => AdaSelSketch.update(sketch, as)
    case _                         => super.update(sketch, as)
  }

}

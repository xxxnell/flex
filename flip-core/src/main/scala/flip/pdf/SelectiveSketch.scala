package flip.pdf

import flip.conf.SelectiveSketchConf
import flip.rand.IRng

import scala.language.higherKinds

trait SelectiveSketch[A] extends PeriodicSketch[A] {

  def conf: SelectiveSketchConf

}

trait SelectiveSketchOps[S[_] <: SelectiveSketch[_]] extends PeriodicSketchOps[S] {

  def rebuildCond[A](sketch: S[A]): Boolean = ???

}

object SelectiveSketch extends SelectiveSketchOps[SelectiveSketch] {

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

  // overrides

  override def update[A](sketch: SelectiveSketch[A], as: List[(A, Count)]): SelectiveSketch[A] = sketch match {
    case (sketch: AdaSelSketch[A]) => AdaSelSketch.update(sketch, as)
    case _ => super.update(sketch, as)
  }

}

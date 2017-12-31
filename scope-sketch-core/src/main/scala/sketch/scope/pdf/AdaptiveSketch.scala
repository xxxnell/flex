package sketch.scope.pdf

import sketch.scope.conf.{AdaPerSketchConf, AdaptiveSketchConf}

import scala.collection.immutable.Queue
import scala.language.higherKinds

trait AdaptiveSketch[A] extends Sketch[A] {

  val queue: Queue[(A, Count)]

}

trait AdaptiveSketchOps[S[_]<:AdaptiveSketch[_], C<:AdaptiveSketchConf]
  extends SketchPrimPropOps[S, C]
    with AdaptiveSketchLaws[S, C] {

  def modifyQueue[A](sketch: S[A], f: Queue[(A, Count)] => Queue[(A, Count)]): S[A]

  // overrides

  override def narrowUpdate[A](sketch: S[A], as: List[(A, Count)], conf: C): Option[S[A]] = for {
    sketch1 <- super.narrowUpdate(sketch, as, conf)
    sketch2 = append(sketch1, as, conf: C)
  } yield sketch2

  override def rearrange[A](sketch: S[A], conf: C): Option[S[A]] = {
    deepUpdate(sketch, sketch.queue.toList.asInstanceOf[List[(A, Count)]], conf).map(_._1)
  }

}

trait AdaptiveSketchLaws[S[_]<:AdaptiveSketch[_], C<:AdaptiveSketchConf] { self: AdaptiveSketchOps[S, C] =>

  def append[A](sketch: S[A], as: List[(A, Count)], conf: C): S[A] =
    modifyQueue(sketch, queue => (queue ++ as).take(conf.queueSize))

}

object AdaptiveSketch extends AdaptiveSketchOps[AdaptiveSketch, AdaptiveSketchConf] {

  def modifyQueue[A](sketch: AdaptiveSketch[A],
                     f: Queue[(A, Count)] => Queue[(A, Count)]): AdaptiveSketch[A] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyQueue(sketch, f)
  }

//  def update[A](sketch: AdaptiveSketch[A],
//                as: List[(A, Count)],
//                conf: AdaptiveSketchConf): Option[AdaptiveSketch[A]] = sketch match {
//    case sketch: APSketch[A] => APSketch.update(sketch, as, conf)
//    case _ => narrowUpdate(sketch, as, conf)
//  }

  def modifyStructure[A](sketch: AdaptiveSketch[A],
                         f: Structures => Option[Structures]): Option[AdaptiveSketch[A]] = sketch match {
    case sketch: AdaPerSketch[A] => AdaPerSketch.modifyStructure(sketch, f)
  }

  def sample[A](dist: AdaptiveSketch[A]): (AdaptiveSketch[A], A) = ???

  def update[A](sketch: AdaptiveSketch[A],
                as: List[(A, Count)],
                conf: AdaptiveSketchConf): Option[AdaptiveSketch[A]] = (sketch, conf) match {
    case (sketch: AdaPerSketch[A], conf: AdaPerSketchConf) => AdaPerSketch.update(sketch, as, conf)
    case _ => narrowUpdate(sketch, as, conf)
  }

}
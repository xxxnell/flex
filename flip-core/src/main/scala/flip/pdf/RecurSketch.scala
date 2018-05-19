package flip.pdf

import flip.rand.IRng

import scala.language.higherKinds

/**
  * RecurSketch, or Recurrent Sketch has a stream of thresholds. RecurSketch
  * consumes the thresholds and rebuild itself when its counter for
  * number of data exceeds the threshold.
  * */
trait RecurSketch[A] extends Sketch[A] {

  def count: Count

  def thresholds: Stream[Double]

}

trait RecurSketchOps[S[_] <: RecurSketch[_]] extends SketchPrimPropOps[S] with RecurSketchLaws[S] { self =>

  def modifyThresholds[A](sketch: S[A], f: Stream[Double] => Stream[Double]): S[A]

  def modifyCount[A](sketch: S[A], f: Count => Count): S[A]

  def rebuildCond[A](sketch: S[A]): Boolean

}

trait RecurSketchLaws[S[_] <: RecurSketch[_]] { self: RecurSketchOps[S] =>

  def dropThreshold[A](sketch: S[A]): S[A] = modifyThresholds(sketch, thresholds => thresholds.drop(1))

  def update[A](sketch0: S[A], as: List[(A, Count)]): S[A] = {
    val sketch1 = narrowUpdate[A](sketch0, as)
    val sketch2 = modifyCount(sketch1, count => count + as.map(_._2).sum)
    val nextThreshold = sketch0.thresholds.headOption.getOrElse(Double.PositiveInfinity)
    val cond1 = nextThreshold <= sketch2.count
    val cond2 = cond1 && rebuildCond(sketch2)
    val sketch3 = if (cond1) dropThreshold(sketch2) else sketch2
    val sketch4 = if (cond2) rebuild(sketch3) else sketch3

    sketch4
  }

}

object RecurSketch extends RecurSketchOps[RecurSketch] {

  def modifyRng[A](sketch: RecurSketch[A], f: IRng => IRng): RecurSketch[A] =
    sketch match {
      case periodic: PeriodicSketch[A] => PeriodicSketch.modifyRng(periodic, f)
      case selective: SelectiveSketch[A] => SelectiveSketch.modifyRng(selective, f)
    }

  def modifyStructures[A](sketch: RecurSketch[A], f: Structures => Structures): RecurSketch[A] =
    sketch match {
      case periodic: PeriodicSketch[A] => PeriodicSketch.modifyStructures(periodic, f)
      case selective: SelectiveSketch[A] => SelectiveSketch.modifyStructures(selective, f)
    }

  def modifyThresholds[A](sketch: RecurSketch[A], f: Stream[Double] => Stream[Double]): RecurSketch[A] =
    sketch match {
      case periodic: PeriodicSketch[A] => PeriodicSketch.modifyThresholds(periodic, f)
      case selective: SelectiveSketch[A] => SelectiveSketch.modifyThresholds(selective, f)
    }

  def modifyCount[A](sketch: RecurSketch[A], f: Count => Count): RecurSketch[A] = sketch match {
    case periodic: PeriodicSketch[A] => PeriodicSketch.modifyCount(periodic, f)
    case selective: SelectiveSketch[A] => SelectiveSketch.modifyCount(selective, f)
  }

  def rebuildCond[A](sketch: RecurSketch[A]): Boolean = sketch match {
    case periodic: PeriodicSketch[A] => PeriodicSketch.rebuildCond(periodic)
  }

  // overrides

  override def update[A](sketch: RecurSketch[A], as: List[(A, Count)]): RecurSketch[A] = sketch match {
    case (sketch: PeriodicSketch[A]) => PeriodicSketch.update(sketch, as)
    case _ => super.update(sketch, as)
  }

}

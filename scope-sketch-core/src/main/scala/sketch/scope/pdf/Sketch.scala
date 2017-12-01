package sketch.scope.pdf

import cats.implicits._
import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.HDim
import sketch.scope.plot.{DensityPlot, Plot}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A] extends SampleDist[A] {

  def structures: Structures

}

trait SketchPropOps[S[_]<:Sketch[_]] extends SketchPropLaws[S] with SampleDistPropOps[S] {

  // Read ops

  def count[A](sketch: S[A], from: A, to: A): Option[Double]

  // Update ops

  def modifyStructure[A](sketch: S[A], f: Structures => Option[Structures]): Option[S[A]]

  def narrowUpdate[A](sketch: S[A], as: List[A]): Option[S[A]]

  def deepUpdate[A](sketch: S[A], as: List[A]): Option[(S[A], Structure)]

  //  def clear(sketch: S): S

}

trait SketchPropLaws[S[_]<:Sketch[_]] { self: SketchPropOps[S] =>

  def rearrange[A](sketch: S[A]): Option[S[A]] = deepUpdate(sketch, Nil).map(_._1)

}

object Sketch extends SketchPropOps[Sketch] {

  def apply[A](measure: A => Prim, structure: Structures): Sketch[A] = SimpleSketch(measure, structure)

  /**
    * @param measure  measure, i.e. map from input to double, of Sketch
    * @param caDepth  size of Sketch structure
    * @param caSize   Cmap size.
    * @param coDepth  HCounter depth
    * @param coSize   HCounter size
    * */
  def empty[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): Sketch[A] =
    PeriodicSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  // mapping ops

  def probability[A](dist: Sketch[A], from: A, to: A): Option[Prim] = ???

  def densityPlot(dist: Sketch[_]): Option[DensityPlot] = ???

  def count[A](sketch: Sketch[A], from: A, to: A): Option[Prim] = ???

  def narrowUpdate[A](sketch: Sketch[A], as: List[A]): Option[Sketch[A]] = ???

  def deepUpdate[A](sketch: Sketch[A], as: List[A]): Option[(Sketch[A], Structure)] = ???

  def modifyStructure[A](sketch: Sketch[A], f: Structures => Option[Structures]): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.modifyStructure(sketch, f)
    case _ => SimpleSketch.modifyStructure(sketch, f)
  }

  // syntatic sugars

  def update[A](sketch: Sketch[A], as: List[A]): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[A] => ???
    case _ => narrowUpdate(sketch, as)
  }

}
package sketch.scope.dist

import cats.implicits._
import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.hmap.HDim

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A] extends SampleDist[A] {

  def structure: Structure

}

trait SketchPropOps[S[_]<:Sketch[_]] extends SampleDistPropOps[S] {

  def modifyStructure[A](sketch: S[A], f: Structure => Option[Structure]): Option[S[A]]

  def update[A](sketch: S[A], a: A): Option[S[A]]

  def rearrange[A](sketch: S[A]): Option[S[A]]

}

object Sketch extends SketchPrimPropOps[Sketch] {

  def apply[A](measure: A => Prim, structure: Structure): Sketch[A] = SimpleSketch(measure, structure)

  def empty[A](measure: A => Double, caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): Sketch[A] =
    PeriodicSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  // mapping ops

  def primUpdate[A](sketch: Sketch[A], p: Double): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[A] => PeriodicSketch.primUpdate(sketch, p)
    case _ => SimpleSketch.primUpdate(sketch, p)
  }

//  def clear(sketch: Sketch): Sketch = sketch match {
//    case sketch: PeriodicSketch => PeriodicSketch.clear(sketch)
//  }

  def rearrange[A](sketch: Sketch[A]): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.rearrange(sketch)
    case _ => SimpleSketch.rearrange(sketch)
  }

  def modifyStructure[A](sketch: Sketch[A], f: Structure => Option[Structure]): Option[Sketch[A]] = sketch match {
    case sketch: PeriodicSketch[_] => PeriodicSketch.modifyStructure(sketch, f)
    case _ => SimpleSketch.modifyStructure(sketch, f)
  }

}
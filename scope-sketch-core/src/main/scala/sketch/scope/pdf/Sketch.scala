package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.SketchConf
import sketch.scope.measure.Measure

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  *
  * Sketch Data Structure Interface.
  */
trait Sketch[A] extends SamplingDist[A] {

  def structures: Structures

  def conf: SketchConf

}

trait SketchPropOps[S[_]<:Sketch[_]] extends SketchPropLaws[S] with SamplingDistPropOps[S] {

  // Read ops

  def count[A](sketch: S[A], from: A, to: A): Option[Count]

  def sum(sketch: S[_]): Count

  // Update ops

  def modifyStructure[A](sketch: S[A], f: Structures => Option[Structures]): Option[S[A]]

  def narrowUpdate[A](sketch: S[A], as: List[(A, Count)]): Option[S[A]]

  // todo deepUpdate have to take a conf parameter to construct returned Sketch
  def deepUpdate[A](sketch: S[A], as: List[(A, Count)]): Option[(S[A], Structure)]

  //  def clear(sketch: S): S

}

trait SketchPropLaws[S[_]<:Sketch[_]] { self: SketchPropOps[S] =>

  def rearrange[A](sketch: S[A]): Option[S[A]] = deepUpdate(sketch, Nil).map(_._1)

  def cmapNo(sketch: S[_]): Int = sketch.structures.size

  def cmapSize(sketch: S[_]): Int = (for {
    structure <- sketch.structures.headOption
    (cmap, _) = structure
  } yield cmap.size).getOrElse(0)

  def counterNo(sketch: S[_]): Int = (for {
    structure <- sketch.structures.headOption
    (_, hcounter) = structure
  } yield hcounter.depth).getOrElse(0)

  def counterSize(sketch: S[_]): Int = (for {
    structure <- sketch.structures.headOption
    (_, hcounter) = structure
  } yield hcounter.width).getOrElse(0) // sketch.structures.headOption.map { case (_, hcounter) => hcounter.structures.size }.getOrElse(0)

  def lastCmap(sketch: S[_]): Option[Cmap] = for {
    structure <- sketch.structures.lastOption
    cmap = structure._1
  } yield cmap

}

object Sketch extends SketchPrimPropOps[Sketch] {

  def apply[A](measure: Measure[A], structure: Structures, conf: SketchConf): Sketch[A] =
    SimpleSketch(measure, structure, conf)

  /**
    * @param measure  measure of Sketch
    * */
  def empty[A](implicit measure: Measure[A], conf: SketchConf): Sketch[A] = RecurSketch.empty(measure, conf)

  // mapping ops

  def modifyStructure[A](sketch: Sketch[A], f: Structures => Option[Structures]): Option[Sketch[A]] = sketch match {
    case sketch: RecurSketch[_] => RecurSketch.modifyStructure(sketch, f)
    case _ => SimpleSketch.modifyStructure(sketch, f)
  }

  // syntatic sugars

  def update[A](sketch: Sketch[A], as: List[(A, Count)]): Option[Sketch[A]] = sketch match {
    case sketch: RecurSketch[A] => RecurSketch.update(sketch, as)
    case _ => narrowUpdate(sketch, as)
  }

  def sample[A](sketch: Sketch[A]): (Sketch[A], A) = ???

}
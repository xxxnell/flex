package sketch.scope.pdf

import sketch.scope.conf.SketchConf
import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  */
case class SimpleSketch[A](measure: Measure[A], structures: Structures, conf: SketchConf) extends Sketch[A]

trait SimpleSketchOps extends SketchPrimPropOps[SimpleSketch] {

  def sketch2SimpleSketch[A](sketch: Sketch[A]): SimpleSketch[A] =
    SimpleSketch(sketch.measure, sketch.structures, sketch.conf)

}

object SimpleSketch extends SimpleSketchOps {

  def modifyStructure[A](sketch: SimpleSketch[A], f: Structures => Option[Structures]): Option[SimpleSketch[A]] =
    f(sketch.structures).map(structure => SimpleSketch(sketch.measure, structure, sketch.conf))

  def sample[A](dist: SimpleSketch[A]): (SimpleSketch[A], A) = ???

}

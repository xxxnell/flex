package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.SketchConf
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  */
case class SimpleSketch[A](measure: Measure[A], structures: Structures) extends Sketch[A]

trait SimpleSketchOps extends SketchPrimPropOps[SimpleSketch, SketchConf] {

  def sketch2SimpleSketch[A](sketch: Sketch[A]): SimpleSketch[A] =
    SimpleSketch(sketch.measure, sketch.structures)

}

object SimpleSketch extends SimpleSketchOps {

  def empty[A](implicit measure: Measure[A], conf: SketchConf): SimpleSketch[A] =
    SimpleSketch(measure, conf2Structures(conf))

  def modifyStructure[A](sketch: SimpleSketch[A], f: Structures => Option[Structures]): Option[SimpleSketch[A]] =
    f(sketch.structures).map(structure => SimpleSketch(sketch.measure, structure))

  def update[A](sketch: SimpleSketch[A], as: List[(A, Count)], conf: SketchConf): Option[SimpleSketch[A]] =
    narrowUpdate(sketch, as, conf)

}

package flip.pdf

import flip.conf.SketchConf
import flip.measure.Measure

/**
  * SimpleSketch, or Simple Sketch does not update any structures once after it
  * is initialized. It can also be used as an improved histogram that uses
  * Sketch's algorithm without deepUpdate.
  * */
case class SimpleSketch[A](measure: Measure[A], conf: SketchConf, structures: Structures) extends Sketch[A]

trait SimpleSketchOps extends SketchPrimPropOps[SimpleSketch] {

  def sketch2SimpleSketch[A](sketch: Sketch[A]): SimpleSketch[A] =
    SimpleSketch(sketch.measure, sketch.conf, sketch.structures)

}

object SimpleSketch extends SimpleSketchOps {

  def empty[A](implicit measure: Measure[A], conf: SketchConf): SimpleSketch[A] =
    SimpleSketch(measure, conf, structures(conf))

  def concat[A](as: List[(A, Count)])(implicit measure: Measure[A], conf: SketchConf): SimpleSketch[A] =
    narrowUpdate(SimpleSketch(measure, conf, concatStructures(as, measure, conf)), as)

  def modifyStructure[A](sketch: SimpleSketch[A], f: Structures => Structures): SimpleSketch[A] =
    SimpleSketch(sketch.measure, sketch.conf, f(sketch.structures))

  def update[A](sketch: SimpleSketch[A], as: List[(A, Count)]): SimpleSketch[A] =
    narrowUpdate(sketch, as)

}

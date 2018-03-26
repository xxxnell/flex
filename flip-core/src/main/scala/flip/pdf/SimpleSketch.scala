package flip.pdf

import flip.conf.SketchConf
import flip.measure.Measure
import flip.rand.IRng

/**
  * SimpleSketch, or Simple Sketch does not update any structures once after it
  * is initialized. It can also be used as an improved histogram that uses
  * Sketch's algorithm without deepUpdate.
  * */
case class SimpleSketch[A](measure: Measure[A], rng: IRng, conf: SketchConf, structures: Structures) extends Sketch[A]

trait SimpleSketchOps extends SketchPrimPropOps[SimpleSketch] {

  def sketch2SimpleSketch[A](sketch: Sketch[A]): SimpleSketch[A] =
    SimpleSketch(sketch.measure, sketch.rng, sketch.conf, sketch.structures)

}

object SimpleSketch extends SimpleSketchOps {

  def empty[A](implicit measure: Measure[A], conf: SketchConf): SimpleSketch[A] =
    SimpleSketch(measure, IRng(-1), conf, structures(conf))

  def concat[A](as: List[(A, Count)])(implicit measure: Measure[A], conf: SketchConf): SimpleSketch[A] =
    narrowUpdate(SimpleSketch(measure, IRng(-1), conf, concatStructures(as, measure, conf)), as)

  def modifyRng[A](sketch: SimpleSketch[A], f: IRng => IRng): SimpleSketch[A] =
    SimpleSketch(sketch.measure, f(sketch.rng), sketch.conf, sketch.structures)

  def modifyStructure[A](sketch: SimpleSketch[A], f: Structures => Structures): SimpleSketch[A] =
    SimpleSketch(sketch.measure, sketch.rng, sketch.conf, f(sketch.structures))

  def update[A](sketch: SimpleSketch[A], as: List[(A, Count)]): SimpleSketch[A] =
    narrowUpdate(sketch, as)

}

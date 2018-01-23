package flip.pdf

import flip.conf.SketchConf
import flip.measure.Measure

case class SimpleSketch[A](measure: Measure[A], conf: SketchConf, structures: Structures) extends Sketch[A]

trait SimpleSketchOps extends SketchPrimPropOps[SimpleSketch] {

  def sketch2SimpleSketch[A](sketch: Sketch[A]): SimpleSketch[A] =
    SimpleSketch(sketch.measure, sketch.conf, sketch.structures)

}

object SimpleSketch extends SimpleSketchOps {

  def empty[A](implicit measure: Measure[A], conf: SketchConf): SimpleSketch[A] =
    SimpleSketch(measure, conf, conf2Structures(conf))

  def modifyStructure[A](sketch: SimpleSketch[A], f: Structures => Option[Structures]): Option[SimpleSketch[A]] =
    f(sketch.structures).map(structure => SimpleSketch(sketch.measure, sketch.conf, structure))

  def update[A](sketch: SimpleSketch[A], as: List[(A, Count)]): Option[SimpleSketch[A]] =
    narrowUpdate(sketch, as)

}

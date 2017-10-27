package sketch.scope.sketch

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter

/**
  * Licensed by Probe Technology, Inc.
  */
case class SimpleSketch[A](measure: A => Prim, structure: List[(Cmap, HCounter)]) extends Sketch[A]

trait SimpleSketchOps extends SketchOps[SimpleSketch] {

  def primUpdate[A](sketch: SimpleSketch[A], p: Prim): Option[SimpleSketch[A]] = for {
    sketch <- simpleUpdate(sketch, p)
  } yield sketch2SimpleSketch(sketch)

  def rearrange[A](sketch: SimpleSketch[A]): Option[SimpleSketch[A]] = Some(sketch)

  def sketch2SimpleSketch[A](sketch: Sketch[A]): SimpleSketch[A] = SimpleSketch(sketch.measure, sketch.structure)

}

object SimpleSketch extends SimpleSketchOps

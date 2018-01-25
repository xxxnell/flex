package flip.experiment.ops

import flip._

object DataOps {

  def update[A](initSketch: Sketch[A], dataIdxs: List[(Int, A)]): List[(Int, Sketch[A])] = {
    var tempSketchO: Option[Sketch[A]] = Option(initSketch)
    (0, initSketch) :: dataIdxs.flatMap { case (idx, data) =>
      tempSketchO = tempSketchO.flatMap(_.update(data))
      tempSketchO.map(tempSketch => (idx + 1, tempSketch))
    }
  }

}

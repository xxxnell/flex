package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  */
case class ContSketch[A](measure: Measure[A], structures: List[(Cmap, HCounter)], period: Double)
  extends PeriodicSketch[A] {

  val periods: Stream[Double] = Stream.from(1).map(i => period * i)

}

object ContSketch extends  {

  def empty[A](measure: Measure[A], caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): ContSketch[A] = {
    val structure = (1 to caDepth).toList.map(_ => (Cmap.uniform(caSize), HCounter.empty(coDepth, coSize)))
    ContSketch(measure, structure, 100)
  }

}
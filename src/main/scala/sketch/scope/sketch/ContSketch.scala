package sketch.scope.sketch

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter

/**
  * Licensed by Probe Technology, Inc.
  */
case class ContSketch(structure: List[(Cmap, HCounter)], period: Double) extends PeriodicSketch {

  val periods: Stream[Double] = Stream.from(1).map(i => period * i)

}

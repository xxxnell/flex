package sketch.scope.sketch

import sketch.scope.cmap.Cmap
import sketch.scope.hcounter.HCounter

/**
  * Licensed by Probe Technology, Inc.
  */
case class ContSketch(structure: List[(Cmap, HCounter)], period: Double) extends PeriodicSketch {

  val periods: Stream[Double] = Stream.from(1).map(i => period * i)

}

object ContSketch extends  {

  def empty(caDepth: Int, caSize: Int, coDepth: Int, coSize: Int): ContSketch = {
    val structure = (1 to caDepth).toList.map(_ => (Cmap.uniform(caSize), HCounter.empty(coDepth, coSize)))
    ContSketch(structure, 100)
  }

}
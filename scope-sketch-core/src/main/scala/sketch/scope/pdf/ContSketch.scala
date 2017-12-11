package sketch.scope.pdf

import sketch.scope.cmap.Cmap
import sketch.scope.conf.SketchConf
import sketch.scope.hcounter.HCounter
import sketch.scope.measure.Measure

/**
  * Licensed by Probe Technology, Inc.
  */
case class ContSketch[A](measure: Measure[A], structures: List[(Cmap, HCounter)], conf: SketchConf, period: Double)
  extends PeriodicSketch[A] {

  val periods: Stream[Double] = Stream.from(1).map(i => period * i)

}

object ContSketch extends  {

  def empty[A](implicit measure: Measure[A], conf: SketchConf): ContSketch[A] = {
    val structure = (1 to conf.cmapNo).toList
      .map(_ => (Cmap.uniform(conf.cmapSize), HCounter.empty(conf.counterNo, conf.counterSize)))
    ContSketch(measure, structure, conf, 100)
  }

}
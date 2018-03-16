package flip.pdf.monad

import flip.conf.{SketchConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf._
import flip.pdf.arithmetic._
import flip.range.RangeM

object PointToPointSketchBind extends SketchBind[Sketch, Dist, Sketch, SketchConf] {

  def bind[A, B](sketch: Sketch[A], f: A => Dist[B], measure: Measure[B], conf: SketchConf): Sketch[B] = {
    // bindToDist
    val bindedDist = PointToPointBind.bind(sketch, f, measure, conf)
    val dists = PointToPointBind.weightDists(sketch, f, measure, conf).map(_._2)

    // find sampling points
    val sum = sketch.sum
    val samplesB = bindedDist.sampling.records.map { case (range, pd) => (measure.from(range.cutoffMiddle), pd * sum) }

    Sketch.concat(samplesB)(measure, conf)
  }

}

package flex.pdf.monad

import flex.conf.pdf.SketchConf
import flex.measure.Measure
import flex.pdf._

import scala.collection.mutable.ListBuffer

object PointToPointSketchBind extends SketchBind[Sketch, Dist, Sketch, SketchConf] {

  def bind[A, B](sketch: Sketch[A], f: A => Dist[B], measure: Measure[B], conf: SketchConf): Sketch[B] = {
    val bindedDist = PointToPointBind.bind(sketch, f, measure, conf)
    val sum = sketch.sum
    val cum = bindedDist.cdfSampling.records
    val ps = new ListBuffer[(B, Count)]
    var i = 1
    while (i < cum.length) {
      val (x0, y0) = cum.apply(i - 1)
      val (x1, y1) = cum.apply(i)
      ps.append((measure.from(x0), (y1 - y0) * sum))
      i += 1
    }

    Sketch.concat(ps.toList)(measure, conf)
  }

}

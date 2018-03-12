package flip.pdf.monad

import flip.conf.{SketchConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf._
import flip.pdf.arithmetic._
import flip.range.RangeM

object PointToPointSketchBind extends SketchBind[Sketch, Dist, Sketch, SketchConf] {

  def bind[A, B](sketch: Sketch[A], f: A => Dist[B], measureB: Measure[B], conf: SketchConf): Sketch[B] = {
    // bindToDist
    val bindedDist = PointToPointBind.bind(sketch, f, measureB, conf)
    val dists = PointToPointBind.weightDists(sketch, f, measureB, conf).map(_._2)

    // find sampling points
    val smplPointBs = dists
      .flatMap(dist => PointToPointBind.samplingPoints(dist, conf.bindSampling))
      .sortBy(smpl => measureB.to(smpl))
    val sum = sketch.sum
    val samplesB = smplPointBs
      .sliding(2)
      .toList
      .flatMap {
        case start :: end :: Nil =>
          val domainB = RangeM(start, end)(measureB)
          val prob = bindedDist.probability(domainB.start, domainB.end)
          Some((domainB.middle, sum * prob))
        case _ => None
      }
      .filter { case (mid, _) => !measureB.to(mid).isNaN && !measureB.to(mid).isInfinity }
      .filter { case (_, count) => !count.isNaN }

    Sketch.concat(samplesB)(measureB, conf)
  }

}

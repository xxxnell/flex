package flip.pdf.monad

import flip.conf.{SketchConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf._
import flip.pdf.arithmetic._
import flip.range.RangeM

object PointToPointSketchBind extends SketchBind[Sketch, Dist, Sketch, SketchConf] {

  def bind[A, B](sketch: Sketch[A], f: A => Dist[B], measureB: Measure[B], conf: SketchConf): Sketch[B] = {
    // bindToDist
    val sampling = sketch.sampling
    val weightDists = sampling.records.map {
      case (range, value) =>
        (range.roughLength * value, f(sketch.measure.from(range.middle)) match {
          case dist: DeltaDist[B] =>
            val conf = SmoothDistConf.default
            UniformDist.apply(dist.pole, range.roughLength)(measureB, conf)
          case dist => dist
        })
    }
    val bindedDist = PointToPointBind.bind(sketch, f, measureB, conf)

    // find sampling points
    val smplPointBs = weightDists
      .map(_._2)
      .flatMap(dist => samplingPoints(dist, conf.bindSampling))
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

  def samplingPoints[A](dist: Dist[A], samplingNo: Int): List[A] = dist match {
    case sketch: Sketch[A] =>
      sketch.samplingPoints.flatMap(ps => ps.start :: ps.end :: Nil).distinct
    case delta: DeltaDist[A] =>
      val measure = delta.measure
      val poleP = measure.to(delta.pole)
      val width = delta.conf.delta
      measure.from(poleP - width) :: measure.from(poleP + width) :: Nil
    case numeric: NumericDist[A] =>
      val unit = 1 / (samplingNo.toDouble + 1)
      (0 to samplingNo).toList.map(i => numeric.icdf(i * unit))
  }

}

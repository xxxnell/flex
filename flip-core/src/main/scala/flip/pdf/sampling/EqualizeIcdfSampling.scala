package flip.pdf.sampling
import flip.conf.EqualizedIcdfSamplingConf
import flip.measure.Measure
import flip.range.RangeM

/**
  *
  * c.f. Histogram equalization
  * */
object EqualizeIcdfSampling extends IcdfSampling[EqualizedIcdfSamplingConf] {

  def sampling[A](icdf: Double => A, measure: Measure[A], conf: EqualizedIcdfSamplingConf): List[RangeM[A]] = {
    val size = conf.size
    val corr = conf.boundaryRatio
    val unit = 1.0 / (size.toDouble - 2 + 2 * corr)

    (0 to size).toList
      .map {
        case i if i == 0 => 0
        case i if i == size => 1
        case i => unit * corr + unit * (i - 1)
      }
      .map(p => icdf(p))
      .sliding(2)
      .toList
      .flatMap {
        case q1 :: q2 :: Nil => Some(RangeM(q1, q2)(measure))
        case _ => None
      }
  }

}

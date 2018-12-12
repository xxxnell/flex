package flex.pdf.sampling
import flex.conf.pdf.EqualizeIcdfSamplingConf
import flex.measure.Measure
import flex.range.RangeM

/**
  * @see <a href="https://en.wikipedia.org/wiki/Histogram_equalization">Histogram equalization</a>
  * */
object EqualizeIcdfSampling extends IcdfSampling[EqualizeIcdfSamplingConf] {

  def sampling[A](icdf: Double => A, measure: Measure[A], conf: EqualizeIcdfSamplingConf): List[A] = {
    val size = conf.size // todo bucket size (not sampling number)
    val corr = conf.boundaryRatio
    val unit = 1.0 / (size.toDouble - 2 + 2 * corr)

    (1 until size).toList
      .map(i => unit * corr + unit * (i - 1))
      .map(p => icdf(p))
  }

}

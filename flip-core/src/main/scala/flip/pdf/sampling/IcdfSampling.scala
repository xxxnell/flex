package flip.pdf.sampling

import flip.conf.{EqualizedIcdfSamplingConf, IcdfSamplingConf, SmoothDistConf}
import flip.measure.Measure
import flip.range.RangeM

/**
  * ICDF-dependent sampling method.
  * */
trait IcdfSampling[C <: IcdfSamplingConf] {

  def sampling[A](icdf: Double => A, measure: Measure[A], conf: C): List[RangeM[A]]

}

object IcdfSampling {

  def apply[A](icdf: Double => A, measure: Measure[A], conf: SmoothDistConf): List[RangeM[A]] = conf.sampling match {
    case conf: EqualizedIcdfSamplingConf => EqualizeIcdfSampling.sampling(icdf, measure, conf)
  }

}

package flip.pdf.sampling

import flip.conf.{EqualizeIcdfSamplingConf, IcdfSamplingConf, SmoothDistConf}
import flip.measure.Measure
import flip.range.RangeM

/**
  * ICDF-dependent sampling method.
  * */
trait IcdfSampling[C <: IcdfSamplingConf] extends IcdfSamplingLaws[C] {

  def sampling[A](icdf: Double => A, measure: Measure[A], conf: C): List[RangeM[A]]

}

trait IcdfSamplingLaws[C <: IcdfSamplingConf] { self: IcdfSampling[C] =>

  def samplingF[A](measure: Measure[A], conf: C): (Double => A) => List[RangeM[A]] =
    (icdf: Double => A) => sampling(icdf, measure, conf)

}

object IcdfSampling extends IcdfSampling[IcdfSamplingConf] {

  def sampling[A](icdf: Double => A, measure: Measure[A], conf: IcdfSamplingConf): List[RangeM[A]] = conf match {
    case conf: EqualizeIcdfSamplingConf => EqualizeIcdfSampling.sampling(icdf, measure, conf)
  }

}

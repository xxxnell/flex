package flex.pdf.sampling

import flex.conf.pdf.{ EqualizeIcdfSamplingConf, IcdfSamplingConf }
import flex.measure.Measure
import flex.range.RangeM

/**
 * ICDF-dependent sampling method.
 * */
trait IcdfSampling[C <: IcdfSamplingConf] extends IcdfSamplingLaws[C] {

  def sampling[A](icdf: Double => A, measure: Measure[A], conf: C): List[A]

}

trait IcdfSamplingLaws[C <: IcdfSamplingConf] { self: IcdfSampling[C] =>

  def samplingF[A](measure: Measure[A], conf: C): (Double => A) => List[A] =
    (icdf: Double => A) => sampling(icdf, measure, conf)

  @Deprecated
  def samplingRanges[A](icdf: Double => A, measure: Measure[A], conf: C): List[RangeM[A]] =
    sampling(icdf, measure, conf).sliding(2).toList.flatMap {
      case q1 :: q2 :: Nil => Some(RangeM(q1, q2)(measure))
      case _ => None
    }

}

object IcdfSampling extends IcdfSampling[IcdfSamplingConf] {

  def sampling[A](icdf: Double => A, measure: Measure[A], conf: IcdfSamplingConf): List[A] = conf match {
    case conf: EqualizeIcdfSamplingConf => EqualizeIcdfSampling.sampling(icdf, measure, conf)
  }

}

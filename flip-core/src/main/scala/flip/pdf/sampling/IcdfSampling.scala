package flip.pdf.sampling

import flip.conf.{IcdfSamplingConf, SmoothDistConf}
import flip.measure.Measure
import flip.range.RangeM

/**
  * ICDF-dependent sampling method.
  * */
trait IcdfSampling[C <: IcdfSamplingConf] {

  def sampling[A](icdf: Double => A, measure: Measure[A], conf: C): List[RangeM[A]]

}

object IcdfSampling {

  def apply[A](icdf: Double => A, measure: Measure[A], conf: SmoothDistConf): List[RangeM[A]] = ???

}

package flex.pdf.monad

import flex.conf.pdf.{SamplingDistConf, SmoothDistConf}
import flex.measure.Measure
import flex.pdf.SamplingDist

object CombinatorialSamplingDistFunctor extends SamplingDistFunctor[SamplingDist, SamplingDist, SamplingDistConf] {

  def map[A, B](dist: SamplingDist[A], f: A => B, measureB: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
    SamplingDistBind.dist
      .bind(dist, (a: A) => Pure(f(a), measureB, SmoothDistConf.forDistConf(conf)), measureB, conf)

}

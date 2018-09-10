package flip.chain.monad

import flip.conf.pdf.{SamplingDistConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.SamplingDist

object CombinatorialSamplingDistFunctor extends SamplingDistFunctor[SamplingDist, SamplingDist, SamplingDistConf] {

  def map[A, B](dist: SamplingDist[A], f: A => B, measureB: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
    SamplingDistBind.dist
      .bind(dist, (a: A) => Pure(f(a), measureB, SmoothDistConf.forDistConf(conf)), measureB, conf)

}

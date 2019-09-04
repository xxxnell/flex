package flex.chain.monad

import flex.conf.pdf.{ SamplingDistConf, SmoothDistConf }
import flex.measure.Measure
import flex.pdf.{ Dist, SamplingDist }

object CombinatorialDistFunctor extends DistFunctor[Dist, SamplingDist, SamplingDistConf] {

  override def map[A, B](dist: Dist[A], f: A => B, measureB: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
    DistBind.dist.bind(dist, (a: A) => Pure(f(a), measureB, SmoothDistConf.forDistConf(conf)), measureB, conf)

}

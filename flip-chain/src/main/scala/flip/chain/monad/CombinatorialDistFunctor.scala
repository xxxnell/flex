package flip.chain.monad

import flip.conf.pdf.{SamplingDistConf, SmoothDistConf}
import flip.measure.Measure
import flip.pdf.{Dist, SamplingDist}

object CombinatorialDistFunctor extends DistFunctor[Dist, SamplingDist, SamplingDistConf] {

  override def map[A, B](dist: Dist[A], f: A => B, measureB: Measure[B], conf: SamplingDistConf): SamplingDist[B] =
    DistBind.dist.bind(dist, (a: A) => Pure(f(a), measureB, SmoothDistConf.forDistConf(conf)), measureB, conf)

}

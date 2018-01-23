package flip.pdf.monad

import flip.conf.SamplingDistConf
import flip.measure.Measure
import flip.pdf.{Dist, SamplingDist}

import scala.language.higherKinds

trait SamplingDistBind[SampleD1[_]<:SamplingDist[_], D[_]<:Dist[_], SampleD2[_]<:SamplingDist[_]]
  extends DistBind[SampleD1, D, SampleD2] {

}

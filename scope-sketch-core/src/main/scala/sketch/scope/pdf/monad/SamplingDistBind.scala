package sketch.scope.pdf.monad

import sketch.scope.conf.SamplingDistConf
import sketch.scope.measure.Measure
import sketch.scope.pdf.{Dist, SamplingDist}

import scala.language.higherKinds

trait SamplingDistBind[SampleD1[_]<:SamplingDist[_], D[_]<:Dist[_], SampleD2[_]<:SamplingDist[_], C<:SamplingDistConf]
  extends DistBind[SampleD1, D, SampleD2, C] {

}

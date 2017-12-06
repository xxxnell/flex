package sketch.scope.pdf.monad

import sketch.scope.measure.Measure
import sketch.scope.pdf.{Dist, SampledDist}

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDistBind[SampleD1[_]<:SampledDist[_], D[_]<:Dist[_], SampleD2[_]<:SampledDist[_]]
  extends DistBind[SampleD1, D, SampleD2] {

}

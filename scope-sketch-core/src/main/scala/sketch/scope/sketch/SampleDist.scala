package sketch.scope.sketch

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDist[A] extends Dist[A]

trait SampleDistPropOps[D[_]<:SampleDist[_]] extends DistPropOps[D] {

  def densityPlot(sketch: D[_]): Option[List[(Range, Double)]]

}

package sketch.scope.pdf

import sketch.scope.plot.Plot

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDist[A] extends Dist[A]

trait SampleDistPropOps[D[_]<:SampleDist[_]] extends DistPropOps[D] {

  def densityPlot(dist: D[_]): Option[Plot]

}

object SampleDist extends SampleDistPropOps[SampleDist] {

  def probability[A](dist: SampleDist[A], from: A, to: A): Option[Double] = ???

  def densityPlot(dist: SampleDist[_]): Option[Plot] = ???

}
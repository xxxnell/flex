package sketch.scope.pdf

import sketch.scope.plot.{DensityPlot, Plot}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SampleDist[A] extends Dist[A]

trait SampleDistPropOps[D[_]<:SampleDist[_]] extends DistPropOps[D] {

  def densityPlot(dist: D[_]): Option[DensityPlot]

}

object SampleDist extends SampleDistPropOps[SampleDist] {

//  def forSmoothDist[A](dist: SmoothDist[A], )

  def probability[A](dist: SampleDist[A], from: A, to: A): Option[Double] = dist match {
    case sketch: Sketch[A] => Sketch.probability(sketch, from, to)
    case _ => ???
  }

  def densityPlot(dist: SampleDist[_]): Option[DensityPlot] = dist match {
    case sketch: Sketch[_] => Sketch.densityPlot(sketch)
    case _ => ???
  }

}
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

  def probability[A](dist: SampleDist[A], start: A, end: A): Option[Double] = dist match {
    case sketch: Sketch[A] => Sketch.probability(sketch, start, end)
    case plotted: PlottedDist[A] => PlottedDist.probability(plotted, start, end)
    case _ => ???
  }

  def densityPlot(dist: SampleDist[_]): Option[DensityPlot] = dist match {
    case sketch: Sketch[_] => Sketch.densityPlot(sketch)
    case plotted: PlottedDist[_] => PlottedDist.densityPlot(plotted)
    case _ => ???
  }

}
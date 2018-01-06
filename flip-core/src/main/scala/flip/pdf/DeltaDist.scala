package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.range._

/**
  * Dirac Delta Function.
  */
case class DeltaDist[A](measure: Measure[A], pole: Prim) extends SmoothDist[A]

trait DeltaDistOps extends SmoothDistPropOps[DeltaDist, SmoothDistConf] {

  def probability[A](dist: DeltaDist[A], from: A, to: A): Option[Double] = {
    if(RangeP(dist.measure.to(from), dist.measure.to(to)).contains(dist.pole)) Some(1) else Some(0)
  }

  def pdf[A](dist: DeltaDist[A], a: A): Option[Double] = if(a != 0) Some(0) else None

  def sample[A](dist: DeltaDist[A]): (DeltaDist[A], A) = (dist, dist.measure.from(dist.pole))

}

object DeltaDist extends DeltaDistOps
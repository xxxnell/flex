package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.rand._
import flip.range._

/**
  * Dirac Delta Function.
  */
case class DeltaDist[A](measure: Measure[A], conf: SmoothDistConf, pole: A, rng: IRng = IRng(0))
  extends NumericDist[A]

trait DeltaDistOps extends NumericDistOps[DeltaDist] {

  override def pdf[A](dist: DeltaDist[A], a: A): Option[Double] = {
    val pole = dist.measure.to(dist.pole)

    if(a != pole) Some(0) else Some(Double.PositiveInfinity)
  }

  override def cdf[A](dist: DeltaDist[A], a: A): Option[Double] = {
    val p = dist.measure.to(a)
    val pole = dist.measure.to(dist.pole)

    if(p >= pole) Some(1) else Some(0)
  }

  def icdf[A](dist: DeltaDist[A], p: Double): A = dist.pole

}

object DeltaDist extends DeltaDistOps {

  def modifyRng[A](dist: DeltaDist[A], f: IRng => IRng): DeltaDist[A] =
    DeltaDist(dist.measure, dist.conf, dist.pole, f(dist.rng))

}
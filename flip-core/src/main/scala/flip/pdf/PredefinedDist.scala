package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.rand.IRng

import scala.language.higherKinds

/**
  * PredefinedDist, or Predefined distribution can calculate the probability
  * for arbitrary interval.
  * */
trait PredefinedDist[A] extends SmoothDist[A] {

  def probability(from: A, to: A): Double

}

trait PredefinedDistOps[D[_] <: PredefinedDist[_]] extends SmoothDistPropOps[D] {

  def probability[A](dist: D[A], from: A, to: A): Double =
    dist.asInstanceOf[PredefinedDist[A]].probability(from, to)

}

object PredefinedDist extends PredefinedDistOps[PredefinedDist] {

  case class PredefinedDistImpl[A](measure: Measure[A], rng: IRng, conf: SmoothDistConf, probabilityF: (A, A) => Double)
      extends PredefinedDist[A] {
    def probability(from: A, to: A): Prim = probabilityF(from, to)
  }

  def bare[A](measure: Measure[A], rng: IRng, conf: SmoothDistConf, probability: (A, A) => Double): PredefinedDist[A] =
    PredefinedDistImpl(measure, rng, conf, probability)

  def probability[A](probability: (A, A) => Double)(implicit measure: Measure[A],
                                                    conf: SmoothDistConf): PredefinedDist[A] =
    bare(measure, IRng(-1), conf, probability)

  def sample[A](dist: PredefinedDist[A]): (PredefinedDist[A], A) = ???

  def modifyRng[A](dist: PredefinedDist[A], f: IRng => IRng): PredefinedDist[A] =
    bare(dist.measure, f(dist.rng), dist.conf, dist.probability)

}

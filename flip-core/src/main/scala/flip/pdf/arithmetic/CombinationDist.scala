package flip.pdf.arithmetic

import cats.data.NonEmptyList
import flip.conf.DistConf
import flip.measure.Measure
import flip.pdf.{Dist, DistPropOps}
import flip.rand._

import scala.language.higherKinds

trait CombinationDist[A] extends Dist[A] {

  lazy val normalized: NonEmptyList[(Out, Dist[A])] = CombinationDist.normalizing(components)

  /**
    * @return list of (weight, distribution)
    * */
  def components: NonEmptyList[(Double, Dist[A])]

  def rng: IRng

}

trait CombinationDistOps[D[_] <: CombinationDist[_]] extends DistPropOps[D] {

  type Component[A] = (Double, Dist[A])

  type Components[A] = NonEmptyList[Component[A]]

  def modifyRng[A](dist: D[A], f: IRng => IRng): D[A]

  def modifyComponents[A](dist: D[A], f: NonEmptyList[(Double, Dist[A])] => NonEmptyList[(Double, Dist[A])]): D[A]

  def modifyComponent[A](dist: D[A], idx: Int, f: (Double, Dist[A]) => (Double, Dist[A])): D[A] = {
    val (utdWeight, utdDist) = dist.components.asInstanceOf[Components[A]].toList.apply(idx)

    modifyComponents(
      dist,
      components => NonEmptyList.fromList(components.toList.updated(idx, f(utdWeight, utdDist))).get)
  }

  def probability[A](combi: D[A], start: A, end: A): Double = {
    val components = combi.normalized
    val probs = components.map {
      case (weight, dist) =>
        (weight, dist.asInstanceOf[Dist[A]].probability(start, end))
    }

    probs.map { case (weight, prob) => weight * prob }.toList.sum
  }

  def sample[A](combi: D[A]): (D[A], A) = {
    val components = combi.normalized
    val (utdRng, rnd) = combi.rng.next

    val cumWeightDist = {
      var cum = 0.0
      components.map {
        case (weight, dist) =>
          val prevCum = cum
          cum += weight
          (prevCum, dist)
      }
    }
    val ((_, targetDist), targetIdx) = cumWeightDist.zipWithIndex
      .find { case ((cumWeight, _), _) => cumWeight >= rnd }
      .getOrElse((components.head, 0))

    val (utdDist, sample) = targetDist.asInstanceOf[Dist[A]].sample
    val utdCombi1 = modifyComponent(combi, targetIdx, (weight: Double, _: Dist[A]) => (weight, utdDist))
    val utdCombi2 = modifyRng(utdCombi1, _ => utdRng)

    (utdCombi2, sample)
  }

  override def pdf[A](combi: D[A], a: A): Double = {
    val components = combi.normalized
    val pdfs = components.map {
      case (weight, dist) =>
        (weight, dist.asInstanceOf[Dist[A]].pdf(a))
    }

    pdfs.map { case (weight, prob) => weight * prob }.toList.sum
  }

  override def cdf[A](combi: D[A], a: A): Double = {
    val components = combi.normalized
    val cdfs = components.map {
      case (weight, dist) =>
        (weight, dist.asInstanceOf[Dist[A]].cdf(a))
    }

    cdfs.map { case (weight, prob) => weight * prob }.toList.sum
  }

  def normalizingConstant(weights: NonEmptyList[Double]): Double = 1 / weights.toList.sum

  def normalizing[A](components: NonEmptyList[(Double, Dist[A])]): NonEmptyList[(Double, Dist[A])] = {
    val c = normalizingConstant(components.map(_._1))
    components.map { case (weight, dist) => (weight * c, dist) }
  }

}

object CombinationDist extends CombinationDistOps[CombinationDist] {

  private case class CombinationDistImpl[A](measure: Measure[A],
                                            conf: DistConf,
                                            components: NonEmptyList[(Double, Dist[A])],
                                            rng: IRng)
      extends CombinationDist[A]

  def apply[A](components: (Double, Dist[A])*)(implicit measure: Measure[A], conf: DistConf): CombinationDist[A] = {
    val nonEmptyComponents = NonEmptyList.fromList(components.toList).get

    bare(measure, conf, nonEmptyComponents, IRng(nonEmptyComponents.head._1.hashCode()))
  }

  def apply[A](components: NonEmptyList[(Double, Dist[A])])(implicit measure: Measure[A],
                                                            conf: DistConf): CombinationDist[A] = {
    bare(measure, conf, components, IRng(components.head._1.hashCode()))
  }

  def bare[A](measure: Measure[A], conf: DistConf, components: Components[A], rng: IRng): CombinationDist[A] =
    CombinationDistImpl(measure, conf, components, rng)

  def modifyRng[A](dist: CombinationDist[A], f: IRng => IRng): CombinationDist[A] = {
    bare(dist.measure, dist.conf, dist.components, f(dist.rng))
  }

  def modifyComponents[A](dist: CombinationDist[A], f: Components[A] => Components[A]): CombinationDist[A] = {
    bare(dist.measure, dist.conf, f(dist.components), dist.rng)
  }

}

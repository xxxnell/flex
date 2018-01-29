package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.rand.IRng

trait UniformDist[A] extends NumericDist[A] {
  val scale: A
  val width: Double
}

trait UniformDistOps extends NumericDistOps[UniformDist] {

  override def pdf[A](dist: UniformDist[A], a: A): Option[Prim] = {
    val measure = dist.measure
    val p = measure.to(a)
    val pScale = measure.to(dist.scale)

    if((pScale - dist.width / 2) < p && (pScale + dist.width / 2) > p) {
      Some(1 / dist.width)
    } else {
      Some(0)
    }
  }

  override def cdf[A](dist: UniformDist[A], a: A): Option[Double] = {
    val measure = dist.measure
    val p = measure.to(a)
    val pScale = measure.to(dist.scale)
    val q = p - pScale + (dist.width / 2)

    if(q > 1) Some(1) else if (q < 0) Some(0) else Some(q)
  }

  def icdf[A](dist: UniformDist[A], p: Double): A = {
    if(p >= 0 && p <= 1) {
      val measure = dist.measure
      val pScale = measure.to(dist.scale)

      measure.from(dist.width * p + pScale - (dist.width / 2))
    } else throw new IllegalArgumentException(s"$p is forbidden.")
  }

}

object UniformDist extends UniformDistOps {

  private case class UniformDistImpl[A](measure: Measure[A],
                                        conf: SmoothDistConf,
                                        scale: A,
                                        width: Double,
                                        rng: IRng) extends UniformDist[A]

  def apply[A](scale: A, width: Double, rng: IRng)
              (implicit measure: Measure[A], conf: SmoothDistConf): UniformDist[A] =
    bare(measure, conf, scale, width, rng)

  def apply[A](scale: A, width: Double)
              (implicit measure: Measure[A], conf: SmoothDistConf): UniformDist[A] =
    bare(measure, conf, scale, width, IRng(scale.hashCode() + width.hashCode()))

  def bare[A](measure: Measure[A],
              conf: SmoothDistConf,
              scale: A,
              width: Double,
              rng: IRng): UniformDist[A] = UniformDistImpl(measure, conf, scale, width, rng)

  def modifyRng[A](dist: UniformDist[A], f: IRng => IRng): UniformDist[A] =
    bare(dist.measure, dist.conf, dist.scale, dist.width, f(dist.rng))

}

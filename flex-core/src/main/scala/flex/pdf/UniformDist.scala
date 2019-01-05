package flex.pdf

import flex.conf.pdf.SmoothDistConf
import flex.measure.Measure
import flex.plot.PointPlot
import flex.rand.IRng

/**
 * UniformDist, or Uniform distribution.
 *
 * @see <a href="https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)">
 *        Uniform distribution - Wikipedia</a>
 * */
trait UniformDist[A] extends NumericDist[A] {
  val scale: A
  val width: Double
}

trait UniformDistOps extends NumericDistOps[UniformDist] { self =>

  override def pdf[A](dist: UniformDist[A], a: A): Prim = {
    val measure = dist.measure
    val p = measure.to(a)
    val pScale = measure.to(dist.scale)

    if ((pScale - dist.width / 2) <= p && (pScale + dist.width / 2) >= p) {
      1.0 / dist.width
    } else 0.0
  }

  override def cdf[A](dist: UniformDist[A], a: A): Double = {
    val measure = dist.measure
    val p = measure.to(a)
    val pScale = measure.to(dist.scale)
    val q = p - pScale + (dist.width / 2)

    if (q > 1) 1.0 else if (q < 0) 0.0 else q
  }

  override def icdf[A](dist: UniformDist[A], p: Double): A =
    if (p >= 0 && p <= 1) {
      val measure = dist.measure
      val pScale = measure.to(dist.scale)

      measure.from(dist.width * p + pScale - (dist.width / 2))
    } else throw new IllegalArgumentException(s"$p is forbidden.")

  override def pdfSampling[A](dist: UniformDist[A]): PointPlot = {
    val measure = dist.measure
    val delta = dist.conf.delta
    val width = dist.width
    val scale = measure.to(dist.scale)
    val x1 = scale - width / 2
    val x2 = scale + width / 2
    val pdf = self.pdf(dist, measure.from(x1 / 2 + x2 / 2))

    PointPlot.unsafe(Array.apply((x1 - delta, 0), (x1, pdf), (x2, pdf), (x2 + delta, 0)))
  }

  override def cdfSampling[A](dist: UniformDist[A]): PointPlot = {
    val measure = dist.measure
    val width = dist.width
    val scale = measure.to(dist.scale)
    val x1 = scale - width / 2
    val x2 = scale + width / 2

    PointPlot.unsafe(Array.apply((x1, 0), (x2, 1)))
  }

}

object UniformDist extends UniformDistOps {

  private case class UniformDistImpl[A](measure: Measure[A], conf: SmoothDistConf, scale: A, width: Double, rng: IRng)
      extends UniformDist[A]

  def apply[A](scale: A, width: Double, rng: IRng)(implicit measure: Measure[A], conf: SmoothDistConf): UniformDist[A] =
    bare(measure, conf, scale, width, rng)

  def apply[A](scale: A, width: Double)(implicit measure: Measure[A], conf: SmoothDistConf): UniformDist[A] =
    bare(measure, conf, scale, width, IRng(scale.hashCode() + width.hashCode()))

  def bare[A](measure: Measure[A], conf: SmoothDistConf, scale: A, width: Double, rng: IRng): UniformDist[A] =
    UniformDistImpl(measure, conf, scale, width, rng)

  def modifyRng[A](dist: UniformDist[A], f: IRng => IRng): UniformDist[A] =
    bare(dist.measure, dist.conf, dist.scale, dist.width, f(dist.rng))

}

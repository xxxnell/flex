package flip.pdf

import flip.conf.SmoothDistConf
import flip.measure.Measure
import flip.plot.{DensityPlot, PointPlot}
import flip.rand.IRng

import scala.language.higherKinds

/**
  * NumericDist, or Numeric distribution means that the shape of the
  * distribution - pdf, cdf, and icdf - is defined numerically.
  * */
trait NumericDist[A] extends SmoothDist[A]

trait NumericDistOps[D[_] <: NumericDist[_]] extends SmoothDistPropOps[D] { self =>

  def probability[A](dist: D[A], start: A, end: A): Double = {
    val cdfStart = cdf(dist, start)
    val cdfEnd = cdf(dist, end)

    cdfEnd - cdfStart
  }

}

object NumericDist extends NumericDistOps[NumericDist] {

  // constructor

  def delta[A](pole: A)(implicit measure: Measure[A], conf: SmoothDistConf): DeltaDist[A] =
    DeltaDist(measure, conf, pole)

  def delta[A](pole: A, rng: IRng)(implicit measure: Measure[A], conf: SmoothDistConf): DeltaDist[A] =
    DeltaDist(measure, conf, pole, rng)

  def normal[A](mean: A, variance: Double)(implicit measure: Measure[A], conf: SmoothDistConf): NormalDist[A] =
    NormalDist(mean, variance)

  def normal[A](mean: A, variance: Double, rng: IRng)(implicit measure: Measure[A],
                                                      conf: SmoothDistConf): NormalDist[A] =
    NormalDist(mean, variance, rng)

  def logNormal[A](scale: A, shape: Double)(implicit measure: Measure[A], conf: SmoothDistConf): LogNormalDist[A] =
    LogNormalDist(scale, shape)

  def logNormal[A](scale: A, shape: Double, rng: IRng)(implicit measure: Measure[A],
                                                       conf: SmoothDistConf): LogNormalDist[A] =
    LogNormalDist(scale, shape, rng)

  def pareto[A](scale: A, shape: Double)(implicit measure: Measure[A], conf: SmoothDistConf): ParetoDist[A] =
    ParetoDist(scale, shape)

  def pareto[A](scale: A, shape: Double, rng: IRng)(implicit measure: Measure[A], conf: SmoothDistConf): ParetoDist[A] =
    ParetoDist(scale, shape, rng)

  def uniform[A](scale: A, width: Double)(implicit measure: Measure[A], conf: SmoothDistConf): UniformDist[A] =
    UniformDist(scale, width)

  def uniform[A](scale: A, width: Double, rng: IRng)(implicit measure: Measure[A],
                                                     conf: SmoothDistConf): UniformDist[A] =
    UniformDist(scale, width, rng)

  // pipelining

  def modifyRng[A](dist: NumericDist[A], f: IRng => IRng): NumericDist[A] = dist match {
    case dist: ParetoDist[A] => ParetoDist.modifyRng(dist, f)
    case dist: LogNormalDist[A] => LogNormalDist.modifyRng(dist, f)
    case dist: NormalDist[A] => NormalDist.modifyRng(dist, f)
    case dist: DeltaDist[A] => DeltaDist.modifyRng(dist, f)
    case dist: UniformDist[A] => UniformDist.modifyRng(dist, f)
  }

  override def pdf[A](dist: NumericDist[A], a: A): Prim = dist match {
    case dist: ParetoDist[A] => ParetoDist.pdf(dist, a)
    case dist: LogNormalDist[A] => LogNormalDist.pdf(dist, a)
    case dist: NormalDist[A] => NormalDist.pdf(dist, a)
    case dist: DeltaDist[A] => DeltaDist.pdf(dist, a)
    case dist: UniformDist[A] => UniformDist.pdf(dist, a)
  }

  override def cdf[A](dist: NumericDist[A], a: A): Double = dist match {
    case dist: ParetoDist[A] => ParetoDist.cdf(dist, a)
    case dist: LogNormalDist[A] => LogNormalDist.cdf(dist, a)
    case dist: NormalDist[A] => NormalDist.cdf(dist, a)
    case dist: DeltaDist[A] => DeltaDist.cdf(dist, a)
    case dist: UniformDist[A] => UniformDist.cdf(dist, a)
  }

  override def icdf[A](dist: NumericDist[A], p: Double): A = dist match {
    case dist: ParetoDist[A] => ParetoDist.icdf(dist, p)
    case dist: LogNormalDist[A] => LogNormalDist.icdf(dist, p)
    case dist: NormalDist[A] => NormalDist.icdf(dist, p)
    case dist: DeltaDist[A] => DeltaDist.icdf(dist, p)
    case dist: UniformDist[A] => UniformDist.icdf(dist, p)
  }

  override def sampling[A](dist: NumericDist[A]): PointPlot = dist match {
    case dist: DeltaDist[A] => DeltaDist.sampling(dist)
    case _ => super.sampling(dist)
  }

}

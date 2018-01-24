package flip.sim

import flip.pdf.{Dist, SamplingDist}
import flip.plot.DensityPlot

trait SimSyntax
  extends ConsineSyntax
    with KLDSyntax
    with L2SqSyntax

trait ConsineSyntax {

  private val cosine = flip.sim.Cosine

  def Cosine[A](d1: SamplingDist[A], d2: Dist[A]): Option[Double] =
    cosine(d1, d2).simForSampling(d1, d2)

  def CosineDensity[A](d1: SamplingDist[A], d2: Dist[A]): Option[DensityPlot] =
    cosine(d1, d2).simDensityForSampling(d1, d2)

}

trait KLDSyntax {

  private val kld = flip.sim.KLD

  def KLD[A](d1: SamplingDist[A], d2: Dist[A]): Option[Double] =
    kld.simForSampling(d1, d2)

  def KLDDensity[A](d1: SamplingDist[A], d2: Dist[A]): Option[DensityPlot] =
    kld.simDensityForSampling(d1, d2)

}

trait L2SqSyntax {

  private val l2sq = flip.sim.L2Sq

  def L2Sq[A](d1: SamplingDist[A], d2: Dist[A]): Option[Double] =
    l2sq.simForSampling(d1, d2)

  def L2SqDensity[A](d1: SamplingDist[A], d2: Dist[A]): Option[DensityPlot] =
    l2sq.simDensityForSampling(d1, d2)

  def L2[A](d1: SamplingDist[A], d2: Dist[A]): Option[Double] =
    L2Sq(d1, d2).map(l2sq => math.sqrt(l2sq))

  def Euclidean[A](d1: SamplingDist[A], d2: Dist[A]): Option[Double] = L2(d1, d2)

}
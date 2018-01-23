package flip.sim

import flip.pdf.{Dist, SamplingDist}
import flip.plot.DensityPlot

trait SimSyntax
  extends ConsineSyntax
    with KLDSyntax

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

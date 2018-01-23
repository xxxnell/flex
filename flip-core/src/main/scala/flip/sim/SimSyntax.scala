package flip.sim

import flip.conf.{DistConf, SamplingDistConf}
import flip.pdf.{Dist, PlottedDist, SamplingDist}
import flip.plot.DensityPlot

trait SimSyntax
  extends ConsineSyntax
    with KLDSyntax

trait ConsineSyntax extends CosineSyntax1

trait CosineSyntax1 extends CosineSyntax2 {

  private val cosine = flip.sim.Cosine

  def Cosine[A](d1: SamplingDist[A], conf1: SamplingDistConf, d2: Dist[A], conf2: DistConf): Option[Double] =
    cosine(d1, conf1, d2, conf2).simForSampling(d1, conf1, d2, conf2)

  def CosineDensity[A](d1: SamplingDist[A], conf1: SamplingDistConf, d2: Dist[A], conf2: DistConf): Option[DensityPlot] =
    cosine(d1, conf1, d2, conf2).simDensityForSampling(d1, conf1, d2, conf2)

}

trait CosineSyntax2 {

  private val cosine = flip.sim.Cosine

  def Cosine[A](d1: PlottedDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[Double] =
    cosine(d1, d2, conf2).simForPlotted(d1, d2, conf2)

  def CosineDensity[A](d1: PlottedDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[DensityPlot] =
    cosine(d1, d2, conf2).simDensityForPlotted(d1, d2, conf2)

}

trait KLDSyntax extends KLDSyntax1

trait KLDSyntax1 extends KLDSyntax2 {

  private val kld = flip.sim.KLD

  def KLD[A](d1: SamplingDist[A], conf1: SamplingDistConf, d2: Dist[A], conf2: DistConf): Option[Double] =
    kld.simForSampling(d1, conf1, d2, conf2)

  def KLDDensity[A](d1: SamplingDist[A], conf1: SamplingDistConf, d2: Dist[A], conf2: DistConf): Option[DensityPlot] =
    kld.simDensityForSampling(d1, conf1, d2, conf2)

}

trait KLDSyntax2 {

  private val kld = flip.sim.KLD

  def KLD[A](d1: PlottedDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[Double] =
    kld.simForPlotted(d1, d2, conf2)

  def KLDDensity[A](d1: PlottedDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[DensityPlot] =
    kld.simDensityForPlotted(d1, d2, conf2)

}
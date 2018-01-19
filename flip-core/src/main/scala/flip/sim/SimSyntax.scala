package flip.sim

import flip.conf.DistConf
import flip.pdf.{Dist, SamplingDist}
import flip.plot.DensityPlot

trait SimSyntax extends KLDSyntax

trait KLDSyntax {

  private val kld = flip.sim.KLD

  def KLD[A](d1: SamplingDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[Double] =
    kld.kldForSampling(d1, d2, conf2)

  def KLDDensity[A](d1: SamplingDist[A], d2: Dist[A])(implicit conf2: DistConf): Option[DensityPlot] =
    kld.kldDensityForSampling(d1, d2, conf2)

}
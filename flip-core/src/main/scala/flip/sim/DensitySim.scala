package flip.sim

import flip.conf.{DistConf, SamplingDistConf}
import flip.pdf.{Dist, PlottedDist, Prim, SamplingDist}
import flip.plot.DensityPlot

trait DensitySim {

  def point(value1: Double, value2: Double): Double

  def sim(sampling: Option[DensityPlot],
          pdf: Prim => Double): Option[Double] = for {
    density <- simDensity(sampling, pdf)
    domain <- density.domain
  } yield density.integral(domain.start, domain.end)

  def simDensity[A](sampling: Option[DensityPlot],
                    pdf: Prim => Double): Option[DensityPlot] = for {
    plot1 <- sampling
    densityPlot = plot1.modify { case (range, value) => point(value, pdf(range.middle))}
  } yield densityPlot

  def simForSampling[A](d1: SamplingDist[A], conf1: SamplingDistConf,
                        d2: Dist[A], conf2: DistConf): Option[Double] =
    sim(d1.sampling(conf1), p => d2.pdf(d2.measure.from(p))(conf2).getOrElse(0.0))

  def simDensityForSampling[A](d1: SamplingDist[A], conf1: SamplingDistConf,
                               d2: Dist[A], conf2: DistConf): Option[DensityPlot] =
    simDensity(d1.sampling(conf1), p => d2.pdf(d2.measure.from(p))(conf2).getOrElse(0.0))

  def simForPlotted[A](d1: PlottedDist[A], d2: Dist[A], conf2: DistConf): Option[Double] =
    sim(Some(d1.sampling), p => d2.pdf(d2.measure.from(p))(conf2).getOrElse(0.0))

  def simDensityForPlotted[A](d1: PlottedDist[A], d2: Dist[A], conf2: DistConf): Option[DensityPlot] =
    simDensity(Some(d1.sampling), p => d2.pdf(d2.measure.from(p))(conf2).getOrElse(0.0))

}

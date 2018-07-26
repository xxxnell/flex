package flip.sim

import flip.pdf.Dist
import flip.plot.PointPlot

object ED {

  def delta(cdf1: PointPlot, cdf2: PointPlot): PointPlot = {
    val Δ1 = cdf1.map { case (x, y) => (x, y - cdf2.of(x)) }
    val Δ2 = cdf2.map { case (x, y) => (x, cdf1.of(x) - y) }

    PointPlot.safe(Δ1.records ++ Δ2.records)
  }

  def deltaForDist[A](d1: Dist[A], d2: Dist[A]): PointPlot = {
    delta(d1.cdfSampling, d2.cdfSampling)
  }

  def absDelta(cdf1: PointPlot, cdf2: PointPlot): PointPlot = {
    delta(cdf1, cdf2).map { case (x, y) => (x, math.abs(y)) }
  }

  def absDeltaForDist[A](d1: Dist[A], d2: Dist[A]): PointPlot = {
    absDelta(d1.cdfSampling, d2.cdfSampling)
  }

  def avgDelta(cdf1: PointPlot, cdf2: PointPlot): Double = {
    val Δ = PointPlot.unsafe(absDelta(cdf1, cdf2).records.filter { case (_, y) => y > 0.0 })
    Δ.integralAll / Δ.domain.cutoffLength
  }

  def sim(cdf1: PointPlot, cdf2: PointPlot): Double = {
    val Δrep = avgDelta(cdf1, cdf2)
    1 / (1 / math.abs(Δrep) - 1)
  }

  def simForDist[A](d1: Dist[A], d2: Dist[A]): Double = {
    sim(d1.cdfSampling, d2.cdfSampling)
  }

}

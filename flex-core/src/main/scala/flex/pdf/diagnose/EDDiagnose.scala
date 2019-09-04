package flex.pdf.diagnose

import flex.plot.PointPlot

object EDDiagnose extends CDFDiagnose {

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = {
    var acc = 0.0
    val card = cdf1.records.last._1 - cdf1.records.head._1

    val res = cdfExistsUni(cdf1, cdf2, {
      case (x1, x2, cum11, cum12, cum21, cum22) =>
        val Δ1 = math.abs(cum12 - cum22)
        val Δ2 = math.abs(cum11 - cum21)
        acc += (x2 - x1) * math.abs(Δ1 + Δ2) / (2 * card)
        1 / (1 / acc - 1) >= threshold
    })
    res
  }

}

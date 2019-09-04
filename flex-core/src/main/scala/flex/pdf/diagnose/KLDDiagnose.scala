package flex.pdf.diagnose

import flex.plot.PointPlot

object KLDDiagnose extends CDFDiagnose {

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = {
    var kldacc = 0.0
    cdfExistsItc(cdf1, cdf2, {
      case (x1, x2, cum11, cum12, cum21, cum22) =>
        val d = (cum12 - cum11) * math.log((cum12 - cum11) / (cum22 - cum21))
        kldacc += (if (!d.isNegInfinity) d else 0)
        math.abs(kldacc) >= threshold
    })
  }

}

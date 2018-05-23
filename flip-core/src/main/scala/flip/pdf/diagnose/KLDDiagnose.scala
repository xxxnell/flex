package flip.pdf.diagnose

import flip.plot.PointPlot

object KLDDiagnose extends CDFDiagnose {

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = math.abs(cdfKld(cdf1, cdf2)) > threshold

  def cdfKld(plot1: PointPlot, plot2: PointPlot): Double = {
    var kldacc = 0.0
    cdfForeach(plot1, plot2, {
      case (x1, x2, cum11, cum12, cum21, cum22) =>
        val d = (cum12 - cum11) * math.log((cum12 - cum11) / (cum22 - cum21))
        kldacc += (if (!d.isNegInfinity) d else 0)
    })
    kldacc
  }

}

package flip.pdf.diagnose

import flip.plot.PointPlot

object KLDDiagnose extends CDFDiagnose {

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = math.abs(cdfKld(cdf1, cdf2)) > threshold

  def cdfKld(plot1: PointPlot, plot2: PointPlot): Double = {
    val records = plot1.records
    var i = 1
    var (p, cum1) = records.apply(0)
    var cum2 = plot2.interpolation(p)
    var kldacc = 0.0

    while (i < records.length) {
      val (_p, _cum1) = records.apply(i)
      val _cum2 = plot2.interpolation(_p)
      val d = (_cum1 - cum1) * math.log((_cum1 - cum1) / (_cum2 - cum2))
      kldacc += (if (!d.isNegInfinity) d else 0)
      p = _p
      cum1 = _cum1
      cum2 = _cum2
      i += 1
    }

    println(kldacc)

    kldacc
  }

}

package flip.pdf.diagnose
import flip.plot.PointPlot

/**
  * Kolmogorov–Smirnov test
  * */
object KSDiagnose extends CDFDiagnose {

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = {
    ksStatistic(cdf1, cdf2) > threshold
  }

  def ksStatistic(cdf1: PointPlot, cdf2: PointPlot): Double = {
    var sup = 0.0
    cdfForeach(cdf1, cdf2, {
      case (x1, x2, cum11, cum12, cum21, cum22) =>
        val diff1 = math.abs(cum12 - cum22)
        val diff2 = math.abs(cum11 - cum21)
        sup = (diff1 :: diff2 :: sup :: Nil).max
    })
    sup
  }

}

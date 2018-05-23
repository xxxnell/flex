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
        sup = (cum12 - cum11 :: cum22 - cum21 :: sup :: Nil).max
    })
    sup
  }

}

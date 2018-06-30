package flip.pdf.diagnose
import flip.plot.PointPlot

/**
  * Kolmogorov–Smirnov test
  * */
object KSDiagnose extends CDFDiagnose {

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = {
    var sup = 0.0
    cdfExistsItc(cdf1, cdf2, {
      case (x1, x2, cum11, cum12, cum21, cum22) =>
        val Δ1 = math.abs(cum12 - cum22)
        val Δ2 = math.abs(cum11 - cum21)
        sup = (Δ1 :: Δ2 :: sup :: Nil).max
        sup >= threshold
    })
  }

}

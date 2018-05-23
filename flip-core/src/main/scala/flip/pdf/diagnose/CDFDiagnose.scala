package flip.pdf.diagnose

import flip.plot.PointPlot

trait CDFDiagnose {

  def apply(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = diagnose(cdf1, cdf2, threshold)

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean

}

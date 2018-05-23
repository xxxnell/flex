package flip.pdf.diagnose

import flip.plot.PointPlot

trait CDFDiagnose {

  def apply(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = diagnose(cdf1, cdf2, threshold)

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean

  /**
    * @param f (x1, x2, cum11, cum12, cum21, cum22)
    * */
  protected def cdfForeach(plot1: PointPlot,
                           plot2: PointPlot,
                           f: ((Double, Double, Double, Double, Double, Double)) => Unit): Unit = {
    val records = plot1.records
    var i = 1
    var (x1, cum11) = records.apply(0)
    var cum21 = plot2.interpolation(x1)

    while (i < records.length) {
      val (x2, cum12) = records.apply(i)
      val cum22 = plot2.interpolation(x2)
      f(x1, x2, cum11, cum12, cum21, cum22)
      x1 = x2
      cum11 = cum12
      cum21 = cum22
      i += 1
    }
  }

}

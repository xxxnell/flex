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
    def referencialInterpolation(plot: PointPlot, x: Double, start: Int): (Double, Int) = {
      var k = start
      var y: Option[Double] = None
      while (k < plot.records.length && y.isEmpty) {
        y = plot.referencialInterpolation(x, k)
        if (y.isEmpty) k += 1
      }
      (y.getOrElse(plot.interpolation(x)), k)
    }

    val records = plot1.records
    var i = 1
    var (x1, cum11) = records.apply(0)
    var (cum21, j) = referencialInterpolation(plot2, x1, 0)

    while (i < records.length) {
      val (x2, cum12) = records.apply(i)
      val (cum22, k) = referencialInterpolation(plot2, x2, j)
      f(x1, x2, cum11, cum12, cum21, cum22)
      x1 = x2
      cum11 = cum12
      cum21 = cum22
      i += 1
      j = k
    }
  }

}

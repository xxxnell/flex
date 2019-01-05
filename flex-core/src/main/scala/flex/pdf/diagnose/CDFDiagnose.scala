package flex.pdf.diagnose

import flex.plot.PointPlot

trait CDFDiagnose {

  def apply(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean = diagnose(cdf1, cdf2, threshold)

  def diagnose(cdf1: PointPlot, cdf2: PointPlot, threshold: Double): Boolean

  private def referencialInterpolation(plot: PointPlot, x: Double, start: Int): (Double, Int) = {
    var k = start
    var y: Option[Double] = None
    while (k < plot.records.length && y.isEmpty) {
      y = plot.referencialInterpolation(x, k)
      if (y.isEmpty) k += 1
    }
    (y.getOrElse(plot.interpolation(x)), k)
  }

  /**
   * Exists loop for intersaction of cdf1 and cdf2
   * @param f (x1, x2, cum11, cum12, cum21, cum22)
   * */
  protected def cdfExistsItc(cdf1: PointPlot,
                             cdf2: PointPlot,
                             f: ((Double, Double, Double, Double, Double, Double)) => Boolean): Boolean = {
    val records = cdf1.records
    var i = 1
    var (x1, cum11) = records.apply(0)
    var (cum21, j) = referencialInterpolation(cdf2, x1, 0)
    var exists = false

    while (i < records.length && !exists) {
      val (x2, cum12) = records.apply(i)
      val (cum22, k) = referencialInterpolation(cdf2, x2, j)
      exists = f(x1, x2, cum11, cum12, cum21, cum22)
      x1 = x2
      cum11 = cum12
      cum21 = cum22
      j = k
      i += 1
    }
    exists
  }

  /**
   * Exists loop for union of cdf1 and cdf2
   * @param f (x1, x2, cum11, cum12, cum21, cum22)
   * */
  protected def cdfExistsUni(cdf1: PointPlot,
                             cdf2: PointPlot,
                             f: ((Double, Double, Double, Double, Double, Double)) => Boolean): Boolean =
    cdfExistsItc(cdf2, cdf1, f) // TODO

}

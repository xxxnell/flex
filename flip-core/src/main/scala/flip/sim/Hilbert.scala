package flip.sim

import flip.pdf.Dist
import flip.plot.PointPlot

/**
  * Set of operations for distribution on Hilbert space.
  * */
object Hilbert {

  def norm[A](d1: Dist[A]): Double = normForPlot(d1.pdfSampling)

  def normForPlot(pdf: PointPlot): Double = {
    val records = pdf.records
    var i = 0
    while (i < records.length) {
      val (x, y) = records(i)
      records.update(i, (x, y * y))
      i += 1
    }
    val normsqr = PointPlot.unsafe(records).integralAll
    math.sqrt(normsqr)
  }

}

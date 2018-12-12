package flex.plot

object ShowPlot {

  /**
    * Delimiter-seperated value
    * */
  def dsvRangePlot(plot: RangePlot, delimiter: String): String = {
    plot.records
      .map { case (range, value) => range.start :: range.end :: value :: Nil }
      .map(datas => datas.mkString(delimiter))
      .mkString("\n")
  }

  def dsvPointPlot(plot: PointPlot, delimiter: String): String = {
    plot.records
      .map { case (point, value) => point :: value :: Nil }
      .map(datas => datas.mkString(delimiter))
      .mkString("\n")
  }

}

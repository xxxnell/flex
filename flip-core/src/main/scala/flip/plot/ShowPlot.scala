package flip.plot

object ShowPlot {

  /**
    * Delimiter-seperated value
    * */
  def dsv(plot: Plot, delimiter: String): String = {
    plot
      .records.map { case (range, value) => range.start :: range.end :: value :: Nil }
      .map(datas => datas.mkString(delimiter))
      .mkString("\n")
  }

}

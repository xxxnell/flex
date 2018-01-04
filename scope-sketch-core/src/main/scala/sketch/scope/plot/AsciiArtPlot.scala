package sketch.scope.plot

import sketch.scope.pdf.Dist
import sketch.scope.range.RangeM

object AsciiArtPlot {

  def histogram[A](dist: Dist[A], ranges: List[RangeM[A]]): String = {
    val rangeMaxStrSize = ranges.map(range => range.start.toString.length + range.end.toString.length + 3).max
    val rangeReprSize = if(rangeMaxStrSize <= 10) rangeMaxStrSize else 10

    ranges
      .map(range => (range, dist.probability(range.start, range.end)))
      .flatMap { case (range, probO) => probO.map(prob => (range, prob)) }
      .map { case (range, prob) =>
        s"${asciiForRange(range, rangeReprSize)} ${asciiForProb(prob)} ${asciiProbBarForProb(prob)}"
      }.foldLeft(""){ case (acc, asciiRepr) => acc + asciiRepr + "\n" }
  }

  def asciiForRange[A](range: RangeM[A], length: Int): String = {
    val rangeLength = (length - 3) / 2
    val remLength = length - 3 - rangeLength * 2

    range.start.toString.take(rangeLength) +
      " ~ " +
      range.end.toString.take(rangeLength)
  }

  def asciiForProb(prob: Double): String = {
    prob * 100 + "%"
  }

  def asciiProbBarForProb(prob: Double): String = {
    ("*" * (prob * 100).toInt).take(150)
  }

}

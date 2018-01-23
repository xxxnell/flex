package flip.pdf.monad

import flip.measure.Measure
import flip.pdf._
import flip.pdf.arithmetic._

object PointToPointSketchBind extends SketchBind[Sketch, Dist, Sketch] {

  def bind[A, B](sketch: Sketch[A], f: A => Dist[B], measureB: Measure[B]): Sketch[B] = (for {
    plot <- sketch.densityPlot
    weightDists = plot.records.map { case (range, value) =>
      ((range.length * value).toDouble, f(sketch.measure.from(range.middle)))
    }
    bindedDist = Sum.weightedSum(weightDists, measureB)
    emptySketchB = Sketch.empty(measureB, sketch.conf)
    domainsB = plot.records.map { case (range, _) => (measureB.from(range.start), measureB.from(range.end)) }
    sum = sketch.sum
    records = domainsB.flatMap { domainB =>
      bindedDist.probability(domainB._1, domainB._2).map(prob => (domainB, sum * prob))
    }
    utdSketchBStructure <- Sketch.deepUpdate(emptySketchB, discretizeRecords(records))
    (utdSketchB, structure) = utdSketchBStructure
  } yield utdSketchB)
    .getOrElse(Sketch.empty(measureB, sketch.conf))

  def discretizeRecords[A](records: List[((A, A), Count)]): List[(A, Count)] =
    records.flatMap { case (range, count) => (range._1, count / 2) :: (range._2, count / 2) :: Nil }

}

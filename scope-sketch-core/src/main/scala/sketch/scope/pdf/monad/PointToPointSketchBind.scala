package sketch.scope.pdf.monad

import sketch.scope.measure.Measure
import sketch.scope.range._
import sketch.scope.pdf.{Count, Dist, PredefinedDist, Sketch}

/**
  * Licensed by Probe Technology, Inc.
  */
object PointToPointSketchBind extends SketchBind[Sketch, Dist, Sketch] {

  def bind[A, B](dist: Sketch[A], f: A => Dist[B], measureB: Measure[B]): Sketch[B] = (for {
    plot <- dist.densityPlot
    weightDists = plot.records.map { case (range, value) => (range.length * value, f(dist.measure.from(range.middle))) }
    bindedDist = PredefinedDist[B](measureB, (from: B, to: B) => probabilityForWeightDists(from, to, weightDists))
    emptySketchB = Sketch.empty(measureB, dist.caDepth, dist.caSize, dist.coDepth, dist.coSize)
    domainsB = plot.records.map { case (range, _) => (measureB.from(range.start), measureB.from(range.end)) }
    sum = dist.sum
    records = domainsB.flatMap(domainB =>
      bindedDist.probability(domainB._1, domainB._2).map(prob => (domainB, sum * prob))
    )
    utdSketchBStructure <- Sketch.deepUpdate(emptySketchB, discretizeRecords(records))
    (utdSketchB, structure) = utdSketchBStructure
  } yield utdSketchB)
    .get

  def probabilityForWeightDists[A](from: A, to: A, weightDists: List[(Double, Dist[A])]): Option[Double] =
    weightDists.foldLeft(Option(0d)){ case (accProbO, (weight, distB)) => for {
      accProb <- accProbO
      probB <- distB.probability(from, to)
    } yield accProb + weight * probB }

  def discretizeRecords[A](records: List[((A, A), Count)]): List[(A, Count)] =
    records.flatMap { case (range, count) => (range._1, count / 2) :: (range._2, count / 2) :: Nil }

}

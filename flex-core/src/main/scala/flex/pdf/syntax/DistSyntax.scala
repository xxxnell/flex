package flex.pdf.syntax

import flex.conf.pdf._
import flex.measure.Measure
import flex.pdf.Dist
import flex.pdf.arithmetic.CombinationDist
import flex.plot.PointPlot
import flex.rand._

import scala.language.higherKinds

trait DistSyntax extends DistPropSyntax with DistArthmeticSyntax

// props

trait DistPropSyntax {

  implicit class DistPropSyntaxImpl[A](dist: Dist[A]) {
    def modifyRng(frng: IRng => IRng): Dist[A] = Dist.modifyRng(dist, frng)
    def probability(from: A, to: A): Double = Dist.probability(dist, from, to)
    def pdf(a: A): Double = Dist.pdf(dist, a)
    def cdf(a: A): Double = Dist.cdf(dist, a)
    def icdf(p: Double): A = Dist.icdf(dist, p)
    def sample: (Dist[A], A) = Dist.sample(dist)
    def samples(n: Int): (Dist[A], List[A]) = Dist.samples(dist, n)
    def pdfSampling: PointPlot = Dist.pdfSampling(dist)
    def cdfSampling: PointPlot = Dist.cdfSampling(dist)
  }

}

// arthemetic

trait DistArthmeticSyntax {

  implicit class DistArthmeticSyntaxImpl1[A](weightDist: (Double, Dist[A])) {
    def +(weightDist2: (Double, Dist[A]))(implicit measure: Measure[A], conf: DistConf): CombinationDist[A] =
      CombinationDist.apply(weightDist2, weightDist)
  }

  implicit class DistArthmeticSyntaxImpl2[A](combi: CombinationDist[A]) {
    def +(weightDist2: (Double, Dist[A]))(implicit measure: Measure[A], conf: DistConf): CombinationDist[A] =
      CombinationDist.apply((weightDist2 :: combi.components).toList: _*)
  }

}

package flip.pdf.syntax

import flip.measure.Measure
import flip.pdf.monad.{DistFunctor, DistMonad}
import flip.pdf.{Dist, PlottedDist, SamplingDist, Sketch}
import flip.plot.AsciiArtPlot
import flip.range.RangeM

import scala.language.higherKinds

trait DistSyntax extends DistPropSyntax with DistMonadSyntax

trait DistPropSyntax {

  implicit class DistPropSyntaxImpl[A](dist: Dist[A]) {
    def probability(from: A, to: A): Option[Double] = Dist.probability(dist, from, to)
    def pdf(a: A): Option[Double] = Dist.pdf(dist, a)
    def sample: (Dist[A], A) = Dist.sample(dist)
    def samples(n: Int): (Dist[A], List[A]) = Dist.samples(dist, n)
    def sampling(pltDist: PlottedDist[A]): Option[PlottedDist[A]] =
      Dist.samplingDistForPlottedDist(dist, pltDist)
    def sampling(smplDist: SamplingDist[A]): Option[PlottedDist[A]] =
      Dist.samplingDistForSamplingDist(dist, smplDist)
    def histogram(ranges: List[RangeM[A]]): String = AsciiArtPlot.histogram(dist, ranges)
  }

}

trait DistBindAux[InD[_]<:Dist[_], OutD[_]<:Dist[_]] {
  type Out[A] = OutD[A]
}

trait DistMonadSyntax extends DistMonadSyntax1 {

  implicit class DistMonadSyntaxImpl0[A](dist: Dist[A]) {
    def map[B](f: A => B)
              (implicit
               functor: DistFunctor[Dist],
               measureB: Measure[B]): Dist[B] =
      functor.map(dist, f, measureB)
    def flatMap[B, D1[_]<:Dist[_], D2[_]<:Dist[_]](f: A => D1[B])
                                                  (implicit
                                                   aux1: DistBindAux[D1, D2],
                                                   monad: DistMonad[Dist, D1, D2],
                                                   measureB: Measure[B]): aux1.Out[B] =
      monad.bind(dist, f, measureB)
  }

}

trait DistMonadSyntax1 extends DistMonadSyntax2 {

  implicit def bindAux1: DistBindAux[Sketch, Sketch] = new DistBindAux[Sketch, Sketch] {}
  implicit def distMonad1: DistMonad[Dist, Sketch, Sketch] = DistMonad.sketch

}

trait DistMonadSyntax2 extends DistMonadSyntax3 {

  implicit def bindAux2: DistBindAux[SamplingDist, SamplingDist] = new DistBindAux[SamplingDist, SamplingDist] {}
  implicit def distMonad2: DistMonad[Dist, SamplingDist, SamplingDist] = DistMonad.samplingDist

}

trait DistMonadSyntax3 {

  implicit def bindAux3: DistBindAux[Dist, Dist] = new DistBindAux[Dist, Dist] {}
  implicit def distMonad3: DistMonad[Dist, Dist, Dist] = DistMonad.dist

}

package flip.pdf.syntax

import flip.conf.{DistConf, SamplingDistConf, SketchConf}
import flip.measure.Measure
import flip.pdf.monad.{DistBind, DistFunctor, DistMonad}
import flip.pdf.{Dist, Prim, SamplingDist, Sketch}
import flip.plot.AsciiArtPlot
import flip.range.RangeM

import scala.language.higherKinds

trait DistSyntax extends DistPropSyntax with DistMonadSyntax

trait DistPropSyntax {

  implicit class DistPropSyntaxImpl[A](dist: Dist[A]) {
    def probability(from: A, to: A): Option[Double] = Dist.probability(dist, from, to)
    def pdf(a: A)(implicit conf: DistConf): Option[Double] = Dist.pdf(dist, a, conf)
    def sample: (Dist[A], A) = Dist.sample(dist)
    def samples(n: Int): (Dist[A], List[A]) = Dist.samples(dist, n)
    def histogram(ranges: List[RangeM[A]]): String = AsciiArtPlot.histogram(dist, ranges)
  }

}

trait DistBindAux[InD[_]<:Dist[_], OutD[_]<:Dist[_]] {
  type Out[A] = OutD[A]
}

trait DistConfAux[InD[_]<:Dist[_], OutC<:DistConf] {
  type Out = OutC
}

trait DistMonadSyntax extends DistMonadSyntax1 {

  implicit class DistMonadSyntaxImpl0[A](dist: Dist[A]) {
    def map[B](f: A => B)
              (implicit
               functor: DistFunctor[Dist, DistConf],
               measureB: Measure[B],
               conf: DistConf): Dist[B] =
      functor.map(dist, f, measureB, conf)
    def flatMap[B, D1[_]<:Dist[_], D2[_]<:Dist[_], C<:DistConf](f: A => D1[B])
                                                               (implicit
                                                                aux1: DistBindAux[D1, D2],
                                                                aux2: DistConfAux[D2, C],
                                                                monad: DistMonad[Dist, D1, D2, C],
                                                                measureB: Measure[B],
                                                                conf: C): aux1.Out[B] =
      monad.bind(dist, f, measureB, conf)
  }

}

trait DistMonadSyntax1 extends DistMonadSyntax2 {

  implicit def bindAux1: DistBindAux[Sketch, Sketch] = new DistBindAux[Sketch, Sketch] {}
  implicit def confAux1: DistConfAux[Sketch, SketchConf] = new DistConfAux[Sketch, SketchConf] {}
  implicit def distMonad1: DistMonad[Dist, Sketch, Sketch, SketchConf] = DistMonad.sketch

}

trait DistMonadSyntax2 extends DistMonadSyntax3 {

  implicit def bindAux2: DistBindAux[SamplingDist, SamplingDist] = new DistBindAux[SamplingDist, SamplingDist] {}
  implicit def confAux2: DistConfAux[SamplingDist, SamplingDistConf] = new DistConfAux[SamplingDist, SamplingDistConf]{}
  implicit def distMonad2: DistMonad[Dist, SamplingDist, SamplingDist, SamplingDistConf] = DistMonad.samplingDist

}

trait DistMonadSyntax3 {

  implicit def bindAux3: DistBindAux[Dist, Dist] = new DistBindAux[Dist, Dist] {}
  implicit def confAux3: DistConfAux[Dist, DistConf] = new DistConfAux[Dist, DistConf] {}
  implicit def distMonad3: DistMonad[Dist, Dist, Dist, DistConf] = DistMonad.dist

}

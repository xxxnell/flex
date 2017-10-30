package sketch.scope.sketch.syntax

import sketch.scope.sketch.algebra.DistBind
import sketch.scope.sketch.{Dist, SampleDist, Sketch}

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait DistSyntax extends DistMonadSyntax {

  implicit class DistSyntaxImpl[A](dist: Dist[A]) {
    def probability(from: A, to: A): Option[Double] = Dist.probability(dist, from, to)
  }

}

trait DistBindAux[InD[_]<:Dist[_], OutD[_]<:Dist[_]] {
  type Out[A] = OutD[A]
}

trait DistMonadSyntax extends DistMonadSyntax1 {

  implicit class DistMonadSyntaxImpl0[A](dist: Dist[A]) {
    def map[B](f: A => B): Dist[B] = ???
    def flatMap[B, D1[_]<:Dist[_], D2[_]<:Dist[_]](f: A => D1[B])(implicit aux: DistBindAux[D1, D2]): aux.Out[B] = ???
  }

}

trait DistMonadSyntax1 extends DistMonadSyntax2 {

  implicit val bindAux1: DistBindAux[Sketch, Sketch] = new DistBindAux[Sketch, Sketch] {}

}

trait DistMonadSyntax2 extends DistMonadSyntax3 {

  implicit val bindAux2: DistBindAux[SampleDist, SampleDist] = new DistBindAux[SampleDist, SampleDist] {}

}

trait DistMonadSyntax3 {

  implicit val bindAux3: DistBindAux[Dist, Dist] = new DistBindAux[Dist, Dist] {}

}

//trait DistMonadSyntax1 extends DistMonadSyntax3 {
//
//  implicit class DistMonadSyntaxImpl1[A](dist: Dist[A]) {
////    def flatMap[B](f: A => Sketch[B]): Sketch[B] = ???
//    def flatMap[B](f: Sketch[B]): Sketch[B] = ???
//  }
//
//}
//
////trait DistMonadSyntax2 extends DistMonadSyntax3 {
////
////  implicit class DistMonadSyntaxImpl2[A](dist: Dist[A]) {
//////    def flatMap[B](f: A => SampleDist[B]): SampleDist[B] = ???
////  }
////
////}
//
//trait DistMonadSyntax3 {
//
//  implicit class DistMonadSyntaxImpl3[A](dist: Dist[A]) {
////    def flatMap[B](f: A => Dist[B]): Dist[B] = ???
//    def flatMap[B](f: Dist[B]): Dist[B] = ???
//  }
//
//}
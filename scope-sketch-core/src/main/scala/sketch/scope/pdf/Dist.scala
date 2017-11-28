package sketch.scope.pdf

import cats.Monad

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait Dist[A] {

  def measure: A => Prim

}

trait DistPropOps[D[_]<:Dist[_]] {

  def probability[A](dist: D[A], from: A, to: A): Option[Double]

  //  def pdf(sketch: S, a: Double): Option[Double]

  //  def cdf(sketch: S, a: Double): Option[Double]

}

object Dist extends DistPropOps[Dist] {

  def delta[A](implicit measure: A => Prim): Dist[A] = DeltaDist(measure, 0)

  def delta[A](center: A)(implicit measure: A => Prim): Dist[A] = DeltaDist(measure, measure(center))

  def probability[A](dist: Dist[A], from: A, to: A): Option[Double] = dist match {
    case dist: DeltaDist[A] => DeltaDist.probability(dist, from, to)
    case sketch: Sketch[A] => Sketch.probability(sketch, from, to)
  }

  def flatMap[A, B](dist: Dist[A], f: A => Dist[B]): Dist[B] = ???

  def flatMap[A, B](dist: Dist[A], f: A => SampleDist[B]): SampleDist[B] = ???

  def flatMap[A, B](dist: Dist[A], f: A => Sketch[B]): Sketch[B] = ???

}



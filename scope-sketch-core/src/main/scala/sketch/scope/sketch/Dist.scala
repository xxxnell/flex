package sketch.scope.sketch

/**
  * Licensed by Probe Technology, Inc.
  */
trait Dist[A] {

  def measure: A => Prim

}

trait DistOps[D[_]<:Dist[_]] {

  def probability[A](dist: D[A], from: A, to: A): Option[Double]

  //  def pdf(sketch: S, a: Double): Option[Double]

  //  def cdf(sketch: S, a: Double): Option[Double]

}

trait DistSyntax {

  implicit class DistSyntaxImpl[A](dist: Dist[A]) {
    def probability(from: A, to: A): Option[Double] = Dist.probability(dist, from, to)
  }

}

object Dist extends DistOps[Dist] {

  def probability[A](dist: Dist[A], from: A, to: A): Option[Double] = dist match {
    case dist: DeltaDist[A] => DeltaDist.probability(dist, from, to)
    case sketch: Sketch[A] => Sketch.probability(sketch, from, to)
  }

}



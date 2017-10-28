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

  def densityPlot(dist: D[_]): Option[List[(Range, Double)]]

}



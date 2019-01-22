package flex.vec

import cats.implicits._
import flex.pdf._
import flex.rand._
import org.nd4j.linalg.factory.Nd4j

trait VecOps {

  def dim(vec: Vec): Int = vec.shape.foldLeft(1) { case (acc, d) => acc * d.toInt }

}

trait VecSyntax {

  implicit class VecSyntaxImpl(vec: Vec) {
    def dim: Int = Vec.dim(vec)
  }

}

object Vec extends VecOps {

  object syntax extends VecSyntax

  def apply(as: Double*): Vec = apply(as.toList)

  def apply(as: List[Double]): Vec = Nd4j.create(as.toArray)

  /**
   * Random vector from standard normal distribution
   * */
  def std(dim: Int, rng: IRng): (Vec, IRng) = NormalDist(0.0, 1.0, rng).samples(dim).swap.bimap(Vec(_), _.rng)

  /**
   * Random vector from standard normal distribution
   * */
  def std(dim: Int, rng: IRng, n: Int): (List[Vec], IRng) =
    (0 until n).foldRight((List.empty[Vec], rng)) { case (_, (vs, _rng0)) => std(dim, _rng0).leftMap(_ :: vs) }

  def zeros(dim: Int): Vec = Nd4j.zeros(1l, dim)

  def ones(dim: Int): Vec = Nd4j.ones(1l, dim)

}

package flex.vec

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
  def std(dim: Int, rng: IRng): (Vec, IRng) = {
    val (normal1, samples) = NormalDist(0.0, 1.0, rng).samples(dim)
    (Vec(samples), normal1.rng)
  }

  def zeros(dim: Int): Vec = Nd4j.zeros(1l, dim)

}

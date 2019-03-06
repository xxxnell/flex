package flex.vec

import cats.implicits._
import flex.pdf._
import flex.rand._
import org.nd4j.linalg.factory.Nd4j
import scala.reflect.runtime.universe._

trait VecOps {

  def dim(v: Vec): Int = v.shape.foldLeft(1) { case (acc, d) => acc * d.toInt }

  def csv(v: Vec): String = v.data().asDouble().mkString(",")

}

trait VecSyntax {

  implicit class VecSyntaxImpl(v: Vec) {
    def dim: Int = Vec.dim(v)
    def csv: String = Vec.csv(v)
  }

}

object Vec extends VecOps {

  object syntax extends VecSyntax

  def apply[A: TypeTag](as: List[A]): Vec = typeOf[A] match {
    case t if t =:= typeOf[Int] => int(as.asInstanceOf[List[Int]])
    case t if t =:= typeOf[Float] => float(as.asInstanceOf[List[Float]])
    case t if t =:= typeOf[Double] => double(as.asInstanceOf[List[Double]])
  }

  def apply[A: TypeTag](as: A*): Vec = apply(as.toList)

  def int(as: List[Int]): Vec = Nd4j.create(as.map(a => a.toFloat).toArray, Array(as.length, 1))

  def float(as: List[Float]): Vec = Nd4j.create(as.toArray, Array(as.length, 1))

  def double(as: List[Double]): Vec = Nd4j.create(as.toArray, Array(as.length, 1))

  /**
   * Random vector from standard normal distribution
   * */
  def std(dim: Int, rng: IRng): (Vec, IRng) = NormalDist(0.0, 1.0, rng).samples(dim).swap.bimap(Vec(_), _.rng)

  def stds(dim: Int, rng: IRng, n: Int): (List[Vec], IRng) = (0 until n).foldRight((List.empty[Vec], rng)) {
    case (_, (vs, _rng)) => std(dim, _rng).leftMap(v => v :: vs)
  }

  /**
   * Random vector from standard normal distribution
   * */
  def std(dim: Int, rng: IRng, n: Int): (List[Vec], IRng) =
    (0 until n).foldRight((List.empty[Vec], rng)) { case (_, (vs, _rng0)) => std(dim, _rng0).leftMap(_ :: vs) }

  def zeros(dim: Int): Vec = Nd4j.zeros(dim, 1l)

  def ones(dim: Int): Vec = Nd4j.ones(dim, 1l)

}

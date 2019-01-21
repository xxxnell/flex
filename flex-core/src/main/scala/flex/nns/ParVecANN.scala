package flex.nns

import flex.nns.ANN.syntax._
import flex.vec._
import flex.rand._

trait ParVecANN {

  val arrAnns: Vector[VecANN]

  val compMap: Map[Vec, SumVec]

}

trait ParANNOps extends ParANNLaws {

  def patchArrAnns(ann: ParVecANN, arrAnns: Vector[VecANN]): ParVecANN =
    ParVecANN(arrAnns, ann.compMap)

  def patchCompMap(ann: ParVecANN, compMap: Map[Vec, SumVec]): ParVecANN =
    ParVecANN(ann.arrAnns, compMap)

}

trait ParANNLaws { self: ParANNOps =>

  def add(ann: ParVecANN, xs: List[SumVec]): ParVecANN = xs.foldLeft(ann) {
    case (_ann, x) =>
      val arrAnns1 = _ann.arrAnns.zip(x).map { case (arrAnn, xp) => arrAnn.add(xp) }
      val compMap1 = x.foldLeft(_ann.compMap) { case (_m, xp) => _m.+(xp -> x) }
      patchCompMap(patchArrAnns(_ann, arrAnns1), compMap1)
  }

  def remove(ann: ParVecANN, x: SumVec): ParVecANN = {
    val arrAnns1 = ann.arrAnns.zip(x).map { case (arrAnn, xp) => arrAnn.remove(xp) }
    val compMap1 = x.foldLeft(ann.compMap) { case (_m, xp) => _m.-(xp) }
    patchCompMap(patchArrAnns(ann, arrAnns1), compMap1)
  }

  def search(ann: ParVecANN, xp: Vec, i: Int): Option[SumVec] =
    ann.arrAnns.apply(i).search(xp).flatMap(sp => ann.compMap.get(sp))

}

trait ParANNSyntax extends LSHSyntax {

  implicit class ParANNSyntaxImpl(ann: ParVecANN) {
    def add(x: SumVec): ParVecANN = ParVecANN.add(ann, x :: Nil)
    def adds(xs: List[SumVec]): ParVecANN = ParVecANN.add(ann, xs)
    def remove(x: SumVec): ParVecANN = ParVecANN.remove(ann, x)
    def search(xp: Vec, i: Int): Option[SumVec] = ParVecANN.search(ann, xp, i)
  }

}

object ParVecANN extends ParANNOps {

  object syntax extends ParANNSyntax

  private case class ParVecANNImpl(arrAnns: Vector[VecANN], compMap: Map[Vec, SumVec]) extends ParVecANN

  def apply(arrAnns: Vector[VecANN], compMap: Map[Vec, SumVec]): ParVecANN =
    ParVecANNImpl(arrAnns, compMap)

  def empty(l: Int, dims: List[Int], rng: IRng): (ParVecANN, IRng) = {
    val (arrAnns, rng1) = dims.foldLeft((Vector.empty[VecANN], rng)) {
      case ((_parAnns, _rng1), dim) =>
        VecANN.empty(l, dim, _rng1) match { case (_parAnn, _rng2) => (_parAnns.:+(_parAnn), _rng2) }
    }
    (apply(arrAnns, Map.empty[Vec, SumVec]), rng1)
  }

}

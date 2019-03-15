package flex.nns

import flex.nns.ANN.syntax._
import flex.rand._
import flex.util.IdentityHashMap
import flex.util.IdentityHashMap.syntax._
import flex.vec._

trait ParVecANN {

  val arrAnns: Vector[VecANN]

  val compMap: IdentityHashMap[Vec, SumVec]

}

trait ParANNOps extends ParANNLaws {

  def patchArrAnns(ann: ParVecANN, arrAnns: Vector[VecANN]): ParVecANN =
    ParVecANN(arrAnns, ann.compMap)

  def patchCompMap(ann: ParVecANN, compMap: IdentityHashMap[Vec, SumVec]): ParVecANN =
    ParVecANN(ann.arrAnns, compMap)

  def clear(ann: ParVecANN): Unit = ann.arrAnns.foreach(_.clear)

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

  def dims(ann: ParVecANN): List[Int] = ann.arrAnns.map(_.dim).toList

  def unzip(ann: ParVecANN): List[ParVecANN] = {
    import flex.vec.SumVec.syntax._

    val arrAnnss = ann.arrAnns.map(vann => Vector(vann))
    val compMaps = ann.compMap.inner.foldRight(Vector.fill(dims(ann).size)(IdentityHashMap.empty[Vec, SumVec])) {
      case ((eqv, sv), acc) =>
        val i = sv.eqfy.indexOf(eqv)
        acc.updated(i, acc(i).add(eqv.a, sv))
    }

    arrAnnss.zip(compMaps).map { case (arrAnns, compMap) => ParVecANN(arrAnns, compMap) }.toList
  }

}

trait ParANNSyntax {

  implicit class ParANNSyntaxImpl(ann: ParVecANN) {
    def patchArrAnns(arrAnns: Vector[VecANN]): ParVecANN = ParVecANN.patchArrAnns(ann, arrAnns)
    def add(x: SumVec): ParVecANN = ParVecANN.add(ann, x :: Nil)
    def adds(xs: List[SumVec]): ParVecANN = ParVecANN.add(ann, xs)
    def remove(x: SumVec): ParVecANN = ParVecANN.remove(ann, x)
    def search(xp: Vec, i: Int): Option[SumVec] = ParVecANN.search(ann, xp, i)
    def dims: List[Int] = ParVecANN.dims(ann)
    def clear: Unit = ParVecANN.clear(ann)
    def unzip: List[ParVecANN] = ParVecANN.unzip(ann)
  }

}

object ParVecANN extends ParANNOps {

  object syntax extends ParANNSyntax

  private case class ParVecANNImpl(arrAnns: Vector[VecANN], compMap: IdentityHashMap[Vec, SumVec]) extends ParVecANN

  def apply(arrAnns: Vector[VecANN], compMap: IdentityHashMap[Vec, SumVec]): ParVecANN =
    ParVecANNImpl(arrAnns, compMap)

  def empty(l: Int, dims: List[Int], cache: Int, rng: IRng): (ParVecANN, IRng) = {
    val (arrAnns, rng1) = dims.foldLeft((Vector.empty[VecANN], rng)) {
      case ((_parAnns, _rng1), dim) =>
        VecANN.empty(l, dim, cache, _rng1) match { case (_parAnn, _rng2) => (_parAnns.:+(_parAnn), _rng2) }
    }

    (apply(arrAnns, IdentityHashMap.empty[Vec, SumVec]), rng1)
  }

  def fromSumVecANN(ann: SumVecANN): ParVecANN = {
    val arrAnns = VecANN.fromSumVecANN(ann).toVector
    val compMap = IdentityHashMap(SumVecANN.vs(ann).flatMap(sv => sv.map(v => (v, sv))): _*)

    apply(arrAnns, compMap)
  }

}

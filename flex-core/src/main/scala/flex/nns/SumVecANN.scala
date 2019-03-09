package flex.nns

import flex.vec._
import flex.rand._
import flex.util.{IdentityHashMap, IdentityHashSet}

import scala.collection.immutable.{HashMap, HashSet}

trait SumVecANNOps extends ANNOps[SumVec] {

  def patchHTables(ann: SumVecANN, htables: List[SumVecANN#HTable]): SumVecANN =
    SumVecANN(ann.lshs, htables, ann.vtables)

  def patchVTables(ann: SumVecANN, vtables: List[SumVecANN#VTable]): SumVecANN =
    SumVecANN(ann.lshs, ann.htables, vtables)

  def distance(x1: SumVec, x2: SumVec): Float =
    math.sqrt(x1.zip(x2).map { case (_x1, _x2) => math.pow(_x1.distance2(_x2), 2) }.sum).toFloat

  def dims(ann: SumVecANN): List[Int] = ann.lshs.headOption.map(lsh => lsh.a.dims).getOrElse(Nil)

}

trait SumVecANNSyntax {

  implicit class SumVecANNSyntaxImpl(ann: SumVecANN) {
    def dims: List[Int] = SumVecANN.dims(ann)
  }

}

object SumVecANN extends SumVecANNOps {

  object syntax extends SumVecANNSyntax

  private case class SumVecANNImpl(lshs: List[SumVecLSH],
                                   htables: List[SumVecANN#HTable],
                                   vtables: List[SumVecANN#VTable])
      extends SumVecANN

  def apply(lshs: List[SumVecLSH], htables: List[SumVecANN#HTable], vtables: List[SumVecANN#VTable]): SumVecANN =
    SumVecANNImpl(lshs, htables, vtables)

  /**
   * @param l Number of LSHs
   * @param dims Shape of <code>VQH#Codeword</code>
   * */
  def empty(l: Int, dims: List[Int], rng: IRng): (SumVecANN, IRng) = {
    val w = 1.0f
    val (lshs, rng1) = (1 to l).foldRight((List.empty[LSH[SumVec]], rng)) {
      case (_, (_lshs, _rng1)) => SumVecLSH(dims, w, _rng1) match { case (_lsh, _rng2) => (_lsh :: _lshs, _rng2) }
    }
    val htables = List.fill(l)(IdentityHashMap.empty[Int, IdentityHashSet[SumVec]])
    val vtables = List.fill(l)(IdentityHashMap.empty[SumVec, Int])
    (apply(lshs, htables, vtables), rng1)
  }

}

package flex.nns

import flex.rand.IRng
import flex.vec._

import scala.collection.immutable.{HashMap, HashSet}

trait VecANNOps extends ANNOps[Vec] {

  def patchHTables(ann: VecANN, htables: List[VecANN#HTable]): VecANN =
    VecANN(ann.lshs, htables, ann.vtables)

  def patchVTables(ann: VecANN, vtables: List[VecANN#VTable]): VecANN =
    VecANN(ann.lshs, ann.htables, vtables)

  def distance(x1: Vec, x2: Vec): Float = x1.distance2(x2).toFloat

}

object VecANN extends VecANNOps {

  private case class VecANNImpl(lshs: List[VecLSH], htables: List[VecANN#HTable], vtables: List[VecANN#VTable])
      extends VecANN

  def apply(lshs: List[VecLSH], htables: List[VecANN#HTable], vtables: List[VecANN#VTable]): VecANN =
    VecANNImpl(lshs, htables, vtables)

  def empty(l: Int, dim: Int, rng: IRng): (VecANN, IRng) = {
    val w = 1.0f
    val (lshs, rng1) = (1 to l).foldRight((List.empty[VecLSH], rng)) {
      case (_, (_lshs, _rng1)) => VecLSH(dim, w, _rng1) match { case (_lsh, _rng2) => (_lsh :: _lshs, _rng2) }
    }
    val htables = List.fill(l)(HashMap.empty[Int, HashSet[Vec]])
    val vtables = List.fill(l)(HashMap.empty[Vec, Int])
    (apply(lshs, htables, vtables), rng1)
  }

}

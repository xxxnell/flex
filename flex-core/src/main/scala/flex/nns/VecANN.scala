package flex.nns

import flex.rand.IRng
import flex.util.{IdentityHashMap, IdentityHashSet}
import flex.vec._

trait VecANNOps extends ANNOps[Vec] {

  def patchHTables(ann: VecANN, htables: List[VecANN#HTable]): VecANN =
    VecANN(ann.lsh, htables, ann.vtables)

  def patchVTables(ann: VecANN, vtables: List[VecANN#VTable]): VecANN =
    VecANN(ann.lsh, ann.htables, vtables)

  def distance(x1: Vec, x2: Vec): Float = x1.distance2(x2).toFloat

}

object VecANN extends VecANNOps {

  private case class VecANNImpl(lsh: VecLSH, htables: List[VecANN#HTable], vtables: List[VecANN#VTable]) extends VecANN

  def apply(lsh: VecLSH, htables: List[VecANN#HTable], vtables: List[VecANN#VTable]): VecANN =
    VecANNImpl(lsh, htables, vtables)

  def empty(l: Int, dim: Int, rng: IRng): (VecANN, IRng) = {
    val w = List.fill(l)(1.0f)
    val (lsh, rng1) = VecLSH(dim, w, rng)
    val htables = List.fill(l)(IdentityHashMap.empty[Int, IdentityHashSet[Vec]])
    val vtables = List.fill(l)(IdentityHashMap.empty[Vec, Int])
    (apply(lsh, htables, vtables), rng1)
  }

}

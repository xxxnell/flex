package flex.nns

import flex.vec._
import flex.rand._
import flex.util.{IdentityHashMap, IdentityHashSet}
import flex.util.IdentityHashMap.syntax._
import flex.util.IdentityHashSet.syntax._

import scala.util.Try

trait SumVecANN extends ANN[SumVec] {

  val lsh: SumVecLSH

}

trait SumVecANNOps extends ANNOps[SumVec, SumVecANN] {

  def patchHTables(ann: SumVecANN, htables: List[HTable[SumVec]]): SumVecANN =
    SumVecANN(ann.lsh, htables, ann.vtables)

  def patchVTables(ann: SumVecANN, vtables: List[VTable[SumVec]]): SumVecANN =
    SumVecANN(ann.lsh, ann.htables, vtables)

  def distance(x1: SumVec, x2: SumVec): Float =
    math.sqrt(x1.zip(x2).map { case (_x1, _x2) => math.pow(_x1.distance2(_x2), 2) }.sum).toFloat

  def dims(ann: SumVecANN): List[Int] = ann.lsh.dims

  def unzip(ann: SumVecANN): List[VecANN] = VecANN.fromSumVecANN(ann)

}

trait SumVecANNSyntax {

  implicit class SumVecANNSyntaxImpl(_ann: SumVecANN) extends ANNSyntaxImpl[SumVec, SumVecANN] {
    val ops: SumVecANNOps = SumVecANN
    val ann: SumVecANN = _ann
    def dims: List[Int] = SumVecANN.dims(ann)
    def unzip: List[VecANN] = SumVecANN.unzip(ann)
  }

}

object SumVecANN extends SumVecANNOps {

  object syntax extends SumVecANNSyntax

  private case class SumVecANNImpl(lsh: SumVecLSH, htables: List[HTable[SumVec]], vtables: List[VTable[SumVec]])
      extends SumVecANN

  def apply(lsh: SumVecLSH, htables: List[HTable[SumVec]], vtables: List[VTable[SumVec]]): SumVecANN =
    SumVecANNImpl(lsh, htables, vtables)

  /**
   * @param l Number of LSHs
   * @param dims Shape of <code>VQH#Codeword</code>
   * */
  def empty(l: Int, dims: List[Int], cache: Int, rng: IRng): (SumVecANN, IRng) = {
    val w = List.fill(l)(1.0f)
    val (lsh, rng1) = SumVecLSH(dims, w, cache, rng)
    val htables = List.fill(l)(HTable.empty[SumVec])
    val vtables = List.fill(l)(VTable.empty[SumVec])

    (apply(lsh, htables, vtables), rng1)
  }

  def fromVecANN(vann: VecANN): SumVecANN = {
    val lsh = SumVecLSH.fromVecLSH(vann.lsh)
    val htables = vann.htables.map(htable => htable.map { case (hash, vs) => (hash, vs.map(v => SumVec(v))) })
    val vtables = vann.vtables.map(vtable => vtable.map { case (v, hash) => (SumVec(v), hash) })

    apply(lsh, htables, vtables)
  }

}

package flex.nns

import flex.vec._
import flex.rand._
import flex.util.{IdentityHashMap, IdentityHashSet}

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

  def dims(ann: SumVecANN): List[Int] = ann.lsh.a.map(_a => Try(_a.shape.apply(1)).getOrElse(0L).toInt)

}

trait SumVecANNSyntax {

  implicit class SumVecANNSyntaxImpl(_ann: SumVecANN) extends ANNSyntaxImpl[SumVec, SumVecANN] {
    val ops: SumVecANNOps = SumVecANN
    val ann: SumVecANN = _ann
    def dims: List[Int] = SumVecANN.dims(ann)
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
    val memoSize = cache * dims.size
    val (lsh, rng1) = SumVecLSH(dims, w, memoSize, rng)
    val htables = List.fill(l)(IdentityHashMap.empty[Int, IdentityHashSet[SumVec]])
    val vtables = List.fill(l)(IdentityHashMap.empty[SumVec, Int])
    (apply(lsh, htables, vtables), rng1)
  }

}

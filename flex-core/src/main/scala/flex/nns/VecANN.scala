package flex.nns

import flex.rand.IRng
import flex.util.{IdentityHashMap, IdentityHashSet}
import flex.vec._

trait VecANN extends ANN[Vec] {

  val lsh: VecLSH

}

trait VecANNOps extends ANNOps[Vec, VecANN] {

  def patchHTables(ann: VecANN, htables: List[HTable[Vec]]): VecANN =
    VecANN(ann.lsh, htables, ann.vtables)

  def patchVTables(ann: VecANN, vtables: List[VTable[Vec]]): VecANN =
    VecANN(ann.lsh, ann.htables, vtables)

  def distance(x1: Vec, x2: Vec): Float = x1.distance2(x2).toFloat

}

trait VecANNSyntax {

  implicit class VecANNSyntaxImpl(_ann: VecANN) extends ANNSyntaxImpl[Vec, VecANN] {
    val ops: VecANNOps = VecANN
    val ann: VecANN = _ann
  }

}

object VecANN extends VecANNOps {

  private case class VecANNImpl(lsh: VecLSH, htables: List[HTable[Vec]], vtables: List[VTable[Vec]]) extends VecANN

  def apply(lsh: VecLSH, htables: List[HTable[Vec]], vtables: List[VTable[Vec]]): VecANN =
    VecANNImpl(lsh, htables, vtables)

  def empty(l: Int, dim: Int, cache: Int, rng: IRng): (VecANN, IRng) = {
    val w = List.fill(l)(1.0f)
    val memoSize = cache
    val (lsh, rng1) = VecLSH(dim, w, memoSize, rng)
    val htables = List.fill(l)(IdentityHashMap.empty[Int, IdentityHashSet[Vec]])
    val vtables = List.fill(l)(IdentityHashMap.empty[Vec, Int])
    (apply(lsh, htables, vtables), rng1)
  }

  def fromSumVecANN(svann: SumVecANN): List[VecANN] = {
    val anns0 = svann.lsh.unzip.map { lsh =>
      val l = lsh.size
      val htables = List.fill(l)(IdentityHashMap.empty[Int, IdentityHashSet[Vec]])
      val vtables = List.fill(l)(IdentityHashMap.empty[Vec, Int])
      apply(lsh, htables, vtables)
    }
    SumVecANN.vs(svann).foldLeft(anns0) {
      case (anns, sv) => anns.zip(sv).map { case (ann, v) => add(ann, v :: Nil) }
    }
  }

}

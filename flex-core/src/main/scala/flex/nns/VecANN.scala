package flex.nns

import flex.rand.IRng
import flex.util.{ IdentityHashMap, IdentityHashSet }
import flex.util.IdentityHashSet.syntax._
import flex.util.IdentityHashMap.syntax._
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

  def apply(lsh: VecLSH, hvss: List[List[(Vec, Int)]]): VecANN = {
    val vtables = hvss.map(hvs => IdentityHashMap(hvs))
    val htables = hvss.map(hvs => hvs.groupBy { _._2 }.map { case (h, vs) => (h, IdentityHashSet(vs.map(_._1))) })

    apply(lsh, htables, vtables)
  }

  def empty(l: Int, dim: Int, cache: Int, rng: IRng): (VecANN, IRng) = {
    val w = List.fill(l)(1.0f)
    val (lsh, rng1) = VecLSH(dim, w, cache, rng)
    val htables = List.fill(l)(HTable.empty[Vec])
    val vtables = List.fill(l)(VTable.empty[Vec])

    (apply(lsh, htables, vtables), rng1)
  }

  def fromSumVecANN(svann: SumVecANN): List[VecANN] = {
    val dims = SumVecANN.dims(svann)
    val lshs = svann.lsh.unzip
    val hvsss = svann.vtables.map { vtable0 =>
      vtable0.toMap.foldRight(List.fill(dims.size)(List.empty[(Vec, Int)])) {
        case ((sv, h), acc) => sv.zip(acc).map { case (v, hvs) => (v, h) :: hvs }
      }
    }.transpose

    lshs.zip(hvsss).map { case (lsh, hvss) => apply(lsh, hvss) }
  }

}

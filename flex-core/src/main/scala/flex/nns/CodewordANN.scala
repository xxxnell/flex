package flex.nns

import flex.pdf.VQH
import flex.rand._

import scala.collection.immutable.{HashMap, HashSet}

trait CodewordANN extends ANN[VQH#Codeword]

trait CodewordANNOps extends ANNOps[VQH#Codeword, CodewordANN] {

  def patchHTables(ann: CodewordANN, htables: List[CodewordANN#HTable]): CodewordANN =
    CodewordANN(ann.lshs, htables, ann.vtables)

  def patchVTables(ann: CodewordANN, vtables: List[CodewordANN#VTable]): CodewordANN =
    CodewordANN(ann.lshs, ann.htables, vtables)

  def distance(x1: VQH#Codeword, x2: VQH#Codeword): Float =
    math.sqrt(x1.zip(x2).map { case (_x1, _x2) => math.pow(_x1.distance2(_x2), 2) }.sum).toFloat

}

trait CodewordANNSyntax {

  implicit class CodewordANNSyntaxImpl(_ann: CodewordANN) extends AnnSyntaxImpl[VQH#Codeword, CodewordANN] {
    val ann: CodewordANN = _ann
    val ops: ANNOps[VQH#Codeword, CodewordANN] = CodewordANN
  }

}

object CodewordANN extends CodewordANNOps {

  object syntax extends CodewordANNSyntax

  private case class CodewordANNImpl(lshs: List[LSH[VQH#Codeword]],
                                     htables: List[CodewordANN#HTable],
                                     vtables: List[CodewordANN#VTable])
      extends CodewordANN

  def apply(lshs: List[LSH[VQH#Codeword]],
            htables: List[CodewordANN#HTable],
            vtables: List[CodewordANN#VTable]): CodewordANN =
    CodewordANNImpl(lshs, htables, vtables)

  /**
   * @param l Number of LSHs
   * @param dims Shape of <code>VQH#Codeword</code>
   * */
  def empty(l: Int, dims: List[Int], rng: IRng): (CodewordANN, IRng) = {
    val w = 1.0f
    val (lshs, rng1) = (1 to l).foldRight((List.empty[LSH[VQH#Codeword]], rng)) {
      case (_, (_lshs, _rng1)) => LSH.codeword(dims, w, _rng1) match { case (_lsh, _rng2) => (_lsh :: _lshs, _rng2) }
    }
    val htables = List.fill(l)(HashMap.empty[Int, HashSet[VQH#Codeword]])
    val vtables = List.fill(l)(HashMap.empty[VQH#Codeword, Int])
    (apply(lshs, htables, vtables), rng1)
  }

}

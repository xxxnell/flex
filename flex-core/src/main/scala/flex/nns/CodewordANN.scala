package flex.nns

import flex.pdf.VQH
import org.nd4j.linalg.api.ndarray.INDArray

trait CodewordANN extends ANN[VQH#Codeword]

trait CodewordANNOps extends ANNOps[VQH#Codeword, CodewordANN] {

  def patchHTables(ann: CodewordANN, htables: List[CodewordANN#HTable]): CodewordANN =
    CodewordANN(ann.lshs, htables, ann.vtables)

  def patchVTables(ann: CodewordANN, vtables: List[CodewordANN#VTable]): CodewordANN =
    CodewordANN(ann.lshs, ann.htables, vtables)

  def distance(x1: VQH#Codeword, x2: VQH#Codeword): Float =
    math.sqrt(x1.zip(x2).map { case (_x1, _x2) => math.pow(_x1.distance2(_x2), 2) }.sum).toFloat

}

object CodewordANN extends CodewordANNOps {

  private case class CodewordANNImpl(lshs: List[LSH[VQH#Codeword]],
                                     htables: List[CodewordANN#HTable],
                                     vtables: List[CodewordANN#VTable])
      extends CodewordANN

  def apply(lshs: List[LSH[VQH#Codeword]],
            htables: List[CodewordANN#HTable],
            vtables: List[CodewordANN#VTable]): CodewordANN =
    CodewordANNImpl(lshs, htables, vtables)

}

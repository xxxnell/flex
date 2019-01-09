package flex.nns

import org.nd4j.linalg.api.ndarray.INDArray

trait NDArrayANN extends ANN[INDArray]

trait NDArrayANNOps extends ANNOps[INDArray, NDArrayANN] {

  def patchHTables(ann: NDArrayANN, htables: List[NDArrayANN#HTable]): NDArrayANN =
    NDArrayANN(ann.lshs, htables, ann.vtables)

  def patchVTables(ann: NDArrayANN, vtables: List[NDArrayANN#VTable]): NDArrayANN =
    NDArrayANN(ann.lshs, ann.htables, vtables)

  def distance(x1: INDArray, x2: INDArray): Float = x1.distance2(x2).toFloat

}

object NDArrayANN extends NDArrayANNOps {

  private case class NDArrayANNImpl(lshs: List[LSH[INDArray]],
                                    htables: List[NDArrayANN#HTable],
                                    vtables: List[NDArrayANN#VTable])
      extends NDArrayANN

  def apply(lshs: List[LSH[INDArray]], htables: List[NDArrayANN#HTable], vtables: List[NDArrayANN#VTable]): NDArrayANN =
    NDArrayANNImpl(lshs, htables, vtables)

}

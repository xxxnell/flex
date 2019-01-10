package flex.nns

import flex.pdf.NormalDist
import flex.rand.IRng
import org.nd4j.linalg.api.ndarray.INDArray

import scala.collection.immutable.{HashMap, HashSet}

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

  def empty(l: Int, dim: Int, rng: IRng): (NDArrayANN, IRng) = {
    val w = 1.0f
    val (lshs, rng1) = (1 to l).foldRight((List.empty[LSH[INDArray]], rng)) {
      case (_, (_lshs, _rng1)) => LSH.ndarray(dim, w, _rng1) match { case (_lsh, _rng2) => (_lsh :: _lshs, _rng2) }
    }
    val htables = List.fill(l)(HashMap.empty[Int, HashSet[INDArray]])
    val vtables = List.fill(l)(HashMap.empty[INDArray, Int])
    (apply(lshs, htables, vtables), rng1)
  }

}

package flex.nns

import flex.nns.ANN.syntax._
import flex.pdf.VQH
import flex.rand._
import org.nd4j.linalg.api.ndarray.INDArray

trait ParANN {

  val arrAnns: Vector[NDArrayANN]

  val compMap: Map[INDArray, VQH#Codeword]

}

trait ParANNOps extends ParANNLaws {

  def patchArrAnns(ann: ParANN, arrAnns: Vector[NDArrayANN]): ParANN =
    ParANN(arrAnns, ann.compMap)

  def patchCompMap(ann: ParANN, compMap: Map[INDArray, VQH#Codeword]): ParANN =
    ParANN(ann.arrAnns, compMap)

}

trait ParANNLaws { self: ParANNOps =>

  def add(ann: ParANN, x: VQH#Codeword): ParANN = {
    val arrAnns1 = ann.arrAnns.zip(x).map { case (arrAnn, xp) => arrAnn.add(xp) }
    val compMap1 = x.foldLeft(ann.compMap) { case (_m, xp) => _m.+(xp -> x) }
    patchCompMap(patchArrAnns(ann, arrAnns1), compMap1)
  }

  def remove(ann: ParANN, x: VQH#Codeword): ParANN = {
    val arrAnns1 = ann.arrAnns.zip(x).map { case (arrAnn, xp) => arrAnn.remove(xp) }
    val compMap1 = x.foldLeft(ann.compMap) { case (_m, xp) => _m.-(xp) }
    patchCompMap(patchArrAnns(ann, arrAnns1), compMap1)
  }

  def search(ann: ParANN, xp: INDArray, i: Int): Option[VQH#Codeword] =
    ann.arrAnns.apply(i).search(xp).flatMap(sp => ann.compMap.get(sp))

}

trait ParANNSyntax extends LSHSyntax {

  implicit class ParANNSyntaxImpl(ann: ParANN) {
    def add(x: VQH#Codeword): ParANN = ParANN.add(ann, x)
    def remove(x: VQH#Codeword): ParANN = ParANN.remove(ann, x)
    def search(xp: INDArray, i: Int): Option[VQH#Codeword] = ParANN.search(ann, xp, i)
  }

}

object ParANN extends ParANNOps {

  object syntax extends ParANNSyntax

  private case class ParANNImpl(arrAnns: Vector[NDArrayANN], compMap: Map[INDArray, VQH#Codeword]) extends ParANN

  def apply(arrAnns: Vector[NDArrayANN], compMap: Map[INDArray, VQH#Codeword]): ParANN = ParANNImpl(arrAnns, compMap)

  def empty(l: Int, dims: List[Int], rng: IRng): (ParANN, IRng) = {
    val (arrAnns, rng1) = dims.foldLeft((Vector.empty[NDArrayANN], rng)) {
      case ((_parAnns, _rng1), dim) =>
        NDArrayANN.empty(l, dim, _rng1) match { case (_parAnn, _rng2) => (_parAnns.:+(_parAnn), _rng2) }
    }
    (apply(arrAnns, Map.empty[INDArray, VQH#Codeword]), rng1)
  }

}

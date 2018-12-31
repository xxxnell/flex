package flex.pdf

import org.nd4j.linalg.api.ndarray.INDArray

trait VQH {

  type Codebook = List[INDArray]

  /**
    * codebook-vectors
    * */
  val cs: Vector[Codebook]

  /**
    * counts
    * */
  val ns: Vector[Float]

  /**
    * probabilities
    * */
  val ps: Vector[Float]

}

trait VQHOps extends VQHLaws {

  /**
    * Update partial input vectors with its indices.
    * @return (Updated VQH, new codebook-vectors, out codebook-vectors)
    * */
  def parUpdate(vqh: VQH, xs: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codebook], List[VQH#Codebook]) = ???

  def expUpdate(vqh: VQH, xs: List[(VQH#Codebook, Float)]): (VQH, List[VQH#Codebook], List[VQH#Codebook]) = ???

  def parSearch(vqh: VQH, x: INDArray, i: Int): VQH#Codebook = ???

}

trait VQHLaws { self: VQHOps =>

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def parUpdate(xs: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codebook], List[VQH#Codebook]) =
      VQH.parUpdate(vqh, xs)
    def expUpdate(xs: List[(VQH#Codebook, Float)]): (VQH, List[VQH#Codebook], List[VQH#Codebook]) =
      VQH.expUpdate(vqh, xs)
    def parSearch(x: INDArray, i: Int): VQH#Codebook = VQH.parSearch(vqh, x, i)
    def diagnose: Float = VQH.diagnose(vqh)
  }

}

object VQH extends VQHOps {

  object syntax extends VQHSyntax

  private case class VQHImpl(cs: Vector[List[INDArray]], ns: Vector[Float], ps: Vector[Float]) extends VQH

  def apply(): VQH = ???

  def empty: VQH = ???

}

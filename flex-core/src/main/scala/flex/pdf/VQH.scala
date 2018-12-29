package flex.pdf

import org.nd4j.linalg.api.ndarray.INDArray

trait VQH {

  /**
    * codebook-vectors
    * */
  val cs: Vector[INDArray]

  /**
    * counts
    * */
  val ns: Vector[Float]

  /**
    * probabilities
    * */
  val ps: Vector[Float]

}

trait VQHOps {

  def update(vqh: VQH, xs: List[(INDArray, Float)]): VQH = ???

  def search(vqh: VQH, x: INDArray): INDArray = ???

  def diagnose(vqh: VQH): Float = ???

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def update(xs: List[(INDArray, Float)]): VQH = VQH.update(vqh, xs)
    def search(x: INDArray): INDArray = VQH.search(vqh, x)
    def diagnose: Float = VQH.diagnose(vqh)
  }

}

object VQH extends VQHOps {

  object syntax extends VQHSyntax

  private case class VQHImpl(cs: Vector[INDArray], ns: Vector[Float], ps: Vector[Float]) extends VQH

  def apply(): VQH = ???

  def empty: VQH = ???

}

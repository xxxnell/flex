package flex.pdf

import org.nd4j.linalg.api.ndarray.INDArray
import flex.pdf.Bernoulli.syntax._
import flex.rand._

import scala.collection.immutable.HashMap

trait VQH {

  type Codeword = List[INDArray]

  /**
   * (codeword vectors -> counts)
   * */
  val cns: HashMap[Codeword, Float]

  /**
   * total count
   * */
  val ntot: Float

  val rng: IRng

  /**
   * Expected number of codeword vectors
   * */
  val k: Int

}

trait VQHOps {

  def add(vqh: VQH, c: VQH#Codeword, n: Float): VQH =
    VQH(vqh.cns.+((c, n)), vqh.ntot + n, vqh.k, vqh.rng)

  def patch(vqh: VQH, c: VQH#Codeword, n: Float): VQH =
    VQH(vqh.cns.updated(c, n), vqh.ntot - vqh.cns.getOrElse(c, 0f) + n, vqh.k, vqh.rng)

  /**
   * Update partial input vectors with its indices.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def parUpdate(vqh: VQH, xps: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) =
    xps.foldLeft((vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])) {
      case ((_vqh, _cnews, _couts), (x, a, w)) =>
        val (vqh1, cnews1, couts1) = parSearch(vqh, x, a)
          .map(c => singleUpdate(_vqh, c, diffusionExcept(c.updated(a, x), a :: Nil), w))
          .getOrElse(vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])
        (vqh1, cnews1 ++ _cnews, couts1 ++ _couts)
    }

  /**
   * Update input vectors with its indices.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def expUpdate(vqh: VQH, xs: List[(VQH#Codeword, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) =
    xs.foldLeft((vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])) {
      case ((_vqh, _newcs, _couts), (x, w)) =>
        val (vqh1, cnews1, couts1) = expSearch(vqh, x)
          .map(c => singleUpdate(_vqh, c, x, w))
          .getOrElse(vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])
        (vqh1, cnews1 ++ _newcs, couts1 ++ _couts)
    }

  def singleUpdate(vqh: VQH,
                   c: VQH#Codeword,
                   cnew: => VQH#Codeword,
                   w: Float): (VQH, List[VQH#Codeword], List[VQH#Codeword]) = {
    // Step A. Increase the count
    val n = vqh.cns.getOrElse(c, 0f) + w
    val vqh1 = patch(vqh, c, n)

    // Step B. Add a new codeword vector
    val avgpi = 1 / vqh.k.toFloat
    val (berp, p) = Bernoulli(sigmoid(n / vqh.ntot - avgpi), vqh.rng).sample
    val (vqh2, cnews) = if (p == 1) {
      val _vqh = patch(vqh1, c, n / 2)
      (add(_vqh, cnew, n / 2), cnew :: Nil)
      // TODO abruptly forget loop
    } else (vqh1, Nil)

    // Step C. Remove old codeword vectors
    val qtot = vqh2.cns.map { case (_, _n) => sigmoid(avgpi - _n / vqh2.ntot) }.sum
    var (berq, cnnews, couts, ntot) =
      (berp, HashMap.empty[VQH#Codeword, Float], List.empty[VQH#Codeword], 0f)
    vqh2.cns.foreach {
      case (_c, _n) =>
        val (_berq, q) = Bernoulli(sigmoid(avgpi - _n / vqh2.ntot) / qtot, berq.rng).sample
        berq = _berq
        ntot = ntot + _n
        if (q == 1) couts = _c :: couts else cnnews = cnnews.+((_c, _n))
    }

    (VQH(cnnews, ntot, vqh2.k, berq.rng), cnews, couts)
  }

  def parSearch(vqh: VQH, xp: INDArray, i: Int): Option[VQH#Codeword] = ???

  def expSearch(vqh: VQH, x: VQH#Codeword): Option[VQH#Codeword] = ???

  def sigmoid(x: Float): Float = 1 / (1 + math.exp(-1 * x).toFloat)

  // TODO implement (Ito) diffusion
  def diffusion(x: INDArray): INDArray = x

  def diffusionExcept(c: VQH#Codeword, is: List[Int]): VQH#Codeword = c.zipWithIndex.map {
    case (cp, b) => if (is.contains(b)) cp else diffusion(cp)
  }

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def parUpdate(xs: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) = ???
    def expUpdate(xs: List[(VQH#Codeword, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) = ???
    def parSearch(x: INDArray, i: Int): VQH#Codeword = ???
  }

}

object VQH extends VQHOps {

  object syntax extends VQHSyntax

  private case class VQHImpl(cns: HashMap[VQH#Codeword, Float], ntot: Float, k: Int, rng: IRng) extends VQH

  def apply(cns: HashMap[VQH#Codeword, Float], ntot: Float, k: Int, rng: IRng): VQH = VQHImpl(cns, ntot, k, rng)

  def empty(k: Int): VQH = apply(HashMap.empty[VQH#Codeword, Float], 0, k, IRng(k.hashCode))

}

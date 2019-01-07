package flex.pdf

import org.nd4j.linalg.api.ndarray.INDArray
import flex.pdf.Bernoulli.syntax._
import flex.rand._

trait VQH {

  type Codeword = List[INDArray]

  /**
   * (codeword vectors, counts)
   * */
  val cns: Vector[(Codeword, Float)]

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

  def add(vqh: VQH, cn: (VQH#Codeword, Float)): VQH =
    VQH(vqh.cns.+:(cn), vqh.ntot + cn._2, vqh.k, vqh.rng)

  def patch(vqh: VQH, i: Int, cn: (VQH#Codeword, Float)): VQH =
    VQH(vqh.cns.updated(i, cn), vqh.ntot - vqh.cns(i)._2 + cn._2, vqh.k, vqh.rng)

  /**
   * Update partial input vectors with its indices.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def parUpdate(vqh: VQH, xps: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) =
    xps.foldLeft((vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])) {
      case ((_vqh, _newcs, _outcs), (x, a, w)) =>
        val (vqh1, newcs1, outcs1) = parSearch(vqh, x, a)
          .map { case (c, n, i) => singleUpdate(_vqh, c, i, n, diffusionExcept(c.updated(a, x), a :: Nil), w) }
          .getOrElse(vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])
        (vqh1, newcs1 ++ _newcs, outcs1 ++ _outcs)
    }

  /**
   * Update input vectors with its indices.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def expUpdate(vqh: VQH, xs: List[(VQH#Codeword, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) =
    xs.foldLeft((vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])) {
      case ((_vqh, _newcs, _outcs), (x, w)) =>
        val (vqh1, newcs1, outcs1) = expSearch(vqh, x)
          .map { case (c, n, i) => singleUpdate(_vqh, c, i, n, x, w) }
          .getOrElse(vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])
        (vqh1, newcs1 ++ _newcs, outcs1 ++ _outcs)
    }

  def singleUpdate(vqh: VQH,
                   c: VQH#Codeword,
                   i: Int,
                   n: Float,
                   cnew: => VQH#Codeword,
                   w: Float): (VQH, List[VQH#Codeword], List[VQH#Codeword]) = {
    // Step A. Increase the count
    val vqh1 = patch(vqh, i, (c, n + w))

    // Step B. Add a new cookbook vector
    val avgpi = 1 / vqh.k.toFloat
    val (berp, p) = Bernoulli(sigmoid(n / vqh.ntot - avgpi), vqh.rng).sample
    val (vqh2, cnews) = if (p == 1) {
      val _vqh = patch(vqh1, i, (c, n / 2))
      (add(_vqh, (cnew, n / 2)), cnew :: Nil)
      // TODO abruptly forget loop
    } else (vqh1, Nil)

    // Step C. Remove old cookbook vectors
    val qtot = vqh2.cns.map { case (_, m) => sigmoid(avgpi - m / vqh2.ntot) }.sum
    var (berq, q, cns1, cs2) =
      (berp, 0, Vector.empty[(VQH#Codeword, Float)], List.empty[VQH#Codeword])
    for (i <- 0 to vqh2.cns.length) {
      val _cns = vqh2.cns.apply(i)
      val (_berq, _q) =
        Bernoulli(sigmoid(avgpi - _cns._2 / vqh2.ntot) / qtot, berq.rng).sample
      berq = _berq
      q = _q
      if (q == 0) cns1 = cns1.+:(_cns) else cs2 = _cns._1 :: cs2
    }

    (VQH(cns1, cns1.map(_._2).sum, vqh2.k, berq.rng), cnews, cs2)
  }

  /**
   * @return (codeword vector, count, index)
   * */
  def parSearch(vqh: VQH, xp: INDArray, i: Int): Option[(VQH#Codeword, Float, Int)] = ???

  /**
   * @return (codeword vector, count, index)
   * */
  def expSearch(vqh: VQH, x: VQH#Codeword): Option[(VQH#Codeword, Float, Int)] =
    ???

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

  private case class VQHImpl(cns: Vector[(VQH#Codeword, Float)], ntot: Float, k: Int, rng: IRng) extends VQH

  def apply(cns: Vector[(VQH#Codeword, Float)], ntot: Float, k: Int, rng: IRng): VQH = VQHImpl(cns, ntot, k, rng)

  def empty(k: Int): VQH =
    apply(Vector.empty[(VQH#Codeword, Float)], 0, k, IRng(k.hashCode))

}

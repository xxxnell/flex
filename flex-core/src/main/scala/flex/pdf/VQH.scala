package flex.pdf

import org.nd4j.linalg.api.ndarray.INDArray
import flex.pdf.Bernoulli.syntax._
import flex.rand._

trait VQH {

  type Codebook = List[INDArray]

  /**
    * (codebook vectors, counts)
    * */
  val cns: Vector[(Codebook, Float)]

  /**
    * total count
    * */
  val ntot: Float

  /**
    * Expected number of codebook vectors
    * */
  val k: Int

  val rng: IRng

}

trait VQHOps {

  def add(vqh: VQH, cn: (VQH#Codebook, Float)): VQH = {
    VQH(vqh.cns.+:(cn), vqh.ntot + cn._2, vqh.k, vqh.rng)
  }

  def patch(vqh: VQH, i: Int, cn: (VQH#Codebook, Float)): VQH = {
    VQH(vqh.cns.updated(i, cn), vqh.ntot - vqh.cns(i)._2 + cn._2, vqh.k, vqh.rng)
  }

  /**
    * Update partial input vectors with its indices.
    * @return (Updated VQH, new codebook-vectors, out codebook-vectors)
    * */
  def parUpdate(vqh: VQH, xps: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codebook], List[VQH#Codebook]) = {
    xps.foldRight((vqh, List.empty[VQH#Codebook], List.empty[VQH#Codebook])) {
      case (xp, (vqh0, newcs0, outcs0)) =>
        val (vqh1, newcs1, outcs1) = singleParUpdate(vqh0, xp)
        (vqh1, newcs1 ++ newcs0, outcs1 ++ outcs0)
    }
  }

  def singleParUpdate(vqh: VQH, xp: (INDArray, Int, Float)): (VQH, List[VQH#Codebook], List[VQH#Codebook]) = {
    val (x, a, w) = xp
    parSearch(vqh, x, a).fold((vqh, List.empty[VQH#Codebook], List.empty[VQH#Codebook])) {
      case (c, n, i) =>
        // Step A. Increase the count
        val vqh1 = patch(vqh, i, (c, n + w))

        // Step B. Add a new cookbook vector
        val avgpi = 1 / vqh.k.toFloat
        val (berp, p) = Bernoulli(sigmoid(n / vqh.ntot - avgpi), vqh.rng).sample
        val (vqh2, cnews) = if (p == 1) {
          val cnew = c.zipWithIndex.map { case (cp, b) => if (a == b) x else diffusion(cp) }
          val _vqh = patch(vqh1, i, (c, n / 2))
          (add(_vqh, (cnew, n / 2)), cnew :: Nil)
          // TODO abruptly forget loop
        } else (vqh1, Nil)

        // Step C. Remove old cookbook vectors
        val qtot = vqh2.cns.map { case (_, m) => sigmoid(avgpi - m / vqh2.ntot) }.sum
        var (berq, q, cns1, cs2) = (berp, 0, Vector.empty[(VQH#Codebook, Float)], List.empty[VQH#Codebook])
        for (i <- 0 to vqh2.cns.length) {
          val _cns = vqh2.cns.apply(i)
          val (_berq, _q) = Bernoulli(sigmoid(avgpi - _cns._2 / vqh2.ntot) / qtot, berq.rng).sample
          berq = _berq
          q = _q
          if (q == 0) cns1 = cns1.+:(_cns) else cs2 = _cns._1 :: cs2
        }

        (VQH(cns1, cns1.map(_._2).sum, vqh2.k, berq.rng), cnews, cs2)
    }
  }

  def expUpdate(vqh: VQH, xs: List[(VQH#Codebook, Float)]): (VQH, List[VQH#Codebook], List[VQH#Codebook]) = ???

  /**
    * @return (codebook vector, count, index)
    * */
  def parSearch(vqh: VQH, x: INDArray, i: Int): Option[(VQH#Codebook, Float, Int)] = ???

  def expSearch(vqh: VQH, x: INDArray): Option[VQH#Codebook] = ???

  def sigmoid(x: Float): Float = 1 / (1 + math.exp(-1 * x).toFloat)

  // TODO implement (Ito) diffusion
  def diffusion(x: INDArray): INDArray = x

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def parUpdate(xs: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codebook], List[VQH#Codebook]) = ???
    def expUpdate(xs: List[(VQH#Codebook, Float)]): (VQH, List[VQH#Codebook], List[VQH#Codebook]) = ???
    def parSearch(x: INDArray, i: Int): VQH#Codebook = ???
  }

}

object VQH extends VQHOps {

  object syntax extends VQHSyntax

  private case class VQHImpl(cns: Vector[(VQH#Codebook, Float)], ntot: Float, k: Int, rng: IRng) extends VQH

  def apply(cns: Vector[(VQH#Codebook, Float)], ntot: Float, k: Int, rng: IRng): VQH = VQHImpl(cns, ntot, k, rng)

  def empty(k: Int): VQH = apply(Vector.empty[(VQH#Codebook, Float)], 0, k, IRng(k.hashCode))

}

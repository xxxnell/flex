package flex.pdf

import org.nd4j.linalg.api.ndarray.INDArray
import flex.pdf.Bernoulli.syntax._
import flex.rand._
import flex.nns._
import flex.nns.ANN.syntax._
import org.nd4j.linalg.factory.Nd4j

import scala.collection.immutable.HashMap

trait VQH {

  type Codeword = List[INDArray]

  /**
   * (codeword vectors -> counts)
   * */
  val cns: HashMap[Codeword, Float]

  /**
   * Latest codeword vector.
   * */
  val latest: Codeword

  /**
   * Total count.
   * */
  val ntot: Float

  /**
   * Expected number of codeword vectors.
   * */
  val k: Int

  val rng: IRng

  /**
   * NNS index for codeword vectors.
   * */
  val cwAnn: CodewordANN

  /**
   * NNS index for partial vectors.
   * */
  val parAnn: ParANN

}

trait VQHOps {

  def patchCount(vqh: VQH, x: VQH#Codeword, n: Float): VQH = {
    val cns1 = vqh.cns.updated(x, n)
    val ntot1 = vqh.ntot - vqh.cns.getOrElse(x, 0f) + n
    VQH(cns1, vqh.latest, ntot1, vqh.k, vqh.rng, vqh.cwAnn, vqh.parAnn)
  }

  def patchRng(vqh: VQH, rng: IRng): VQH =
    VQH(vqh.cns, vqh.latest, vqh.ntot, vqh.k, rng, vqh.cwAnn, vqh.parAnn)

  def add(vqh: VQH, x: VQH#Codeword, n: Float): VQH = {
    val cns1 = vqh.cns.+((x, n))
    val cwAnn1 = vqh.cwAnn.add(x)
    val parAnn1 = vqh.parAnn.add(x)
    VQH(cns1, x, vqh.ntot + n, vqh.k, vqh.rng, cwAnn1, parAnn1)
  }

  def remove(vqh: VQH, x: VQH#Codeword): VQH = {
    val cns1 = vqh.cns.-(x)
    val cwAnn1 = vqh.cwAnn.remove(x)
    val parAnn1 = vqh.parAnn.remove(x)
    VQH(cns1, vqh.latest, vqh.ntot - vqh.cns.getOrElse(x, 0f), vqh.k, vqh.rng, cwAnn1, parAnn1)
  }

  /**
   * Update partial input vectors.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def parUpdate(vqh: VQH, xps: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) =
    xps.foldLeft((vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])) {
      case ((_vqh, _cnews, _couts), (xp, a, w)) =>
        val cnew: VQH#Codeword => VQH#Codeword = c => diffusionExcept(c.updated(a, xp), a :: Nil)
        val (vqh1, cnews1, couts1) = parSearch(vqh, xp, a)
          .map(_c => singleUpdate(_vqh, _c, cnew(_c), w))
          .getOrElse((add(_vqh, cnew(vqh.latest), w), cnew(vqh.latest) :: Nil, Nil))
        (vqh1, cnews1 ++ _cnews, couts1 ++ _couts)
    }

  /**
   * Update input vectors.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def expUpdate(vqh: VQH, xs: List[(VQH#Codeword, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) =
    xs.foldLeft((vqh, List.empty[VQH#Codeword], List.empty[VQH#Codeword])) {
      case ((_vqh, _newcs, _couts), (x, w)) =>
        val (vqh1, cnews1, couts1) = expSearch(vqh, x)
          .map(c => singleUpdate(_vqh, c, x, w))
          .getOrElse((add(_vqh, x, w), x :: Nil, Nil))
        (vqh1, cnews1 ++ _newcs, couts1 ++ _couts)
    }

  def singleUpdate(vqh: VQH,
                   c: VQH#Codeword,
                   cnew: => VQH#Codeword,
                   w: Float): (VQH, List[VQH#Codeword], List[VQH#Codeword]) = {
    // Step A. Increase the count
    val n = vqh.cns.getOrElse(c, 0f) + w
    val vqh1 = patchCount(vqh, c, n)

    // Step B. Add a new codeword vector
    val avgpi = 1 / vqh.k.toFloat
    println(s"n / vqh.ntot: ${n / vqh.ntot}")
    val (berp, p) = Bernoulli(sigmoid(n / vqh.ntot - avgpi), vqh.rng).sample
    val (vqh2, cnews) = if (p == 1) {
      val _vqh = patchCount(vqh1, c, n / 2)
      (add(_vqh, cnew, n / 2), cnew :: Nil)
      // TODO abruptly forget loop
    } else (vqh1, Nil)

    // Step C. Remove old codeword vectors
    val qtot = vqh2.cns.map { case (_, _n) => sigmoid(avgpi - _n / vqh2.ntot) }.sum
    val (vqh3, berq, couts) = vqh2.cns.foldLeft(vqh2, berp, List.empty[VQH#Codeword]) {
      case ((_vqh0, _berq0, _couts0), (_c, _n)) =>
        val (_berq1, q) = Bernoulli(sigmoid(avgpi - _n / vqh2.ntot) / qtot, _berq0.rng).sample
        val (_vqh1, _couts1) = if (q == 1) (remove(_vqh0, _c), _c :: _couts0) else (_vqh0, _couts0)
        (_vqh1, _berq1, _couts1)
    }

    (patchRng(vqh3, berq.rng), cnews, couts)
  }

  def parSearch(vqh: VQH, xp: INDArray, i: Int): Option[VQH#Codeword] = vqh.parAnn.search(xp, i)

  def expSearch(vqh: VQH, x: VQH#Codeword): Option[VQH#Codeword] = vqh.cwAnn.search(x)

  def sigmoid(x: Float): Float = 1 / (1 + math.exp(-1 * x).toFloat)

  // TODO
  def diffusion(x: INDArray): INDArray = x

  def diffusionExcept(x: VQH#Codeword, is: List[Int]): VQH#Codeword = x.zipWithIndex.map {
    case (cp, b) => if (is.contains(b)) cp else diffusion(cp)
  }

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def add(x: VQH#Codeword, n: Float): VQH = VQH.add(vqh, x, n)
    def remove(x: VQH#Codeword): VQH = VQH.remove(vqh, x)
    def parUpdate(xps: List[(INDArray, Int, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) =
      VQH.parUpdate(vqh, xps)
    def expUpdate(xs: List[(VQH#Codeword, Float)]): (VQH, List[VQH#Codeword], List[VQH#Codeword]) =
      VQH.expUpdate(vqh, xs)
    def parSearch(xp: INDArray, i: Int): Option[VQH#Codeword] = VQH.parSearch(vqh, xp, i)
    def expSearch(x: VQH#Codeword): Option[VQH#Codeword] = VQH.expSearch(vqh, x)
  }

}

object VQH extends VQHOps {

  object syntax extends VQHSyntax

  private case class VQHImpl(cns: HashMap[VQH#Codeword, Float],
                             latest: VQH#Codeword,
                             ntot: Float,
                             k: Int,
                             rng: IRng,
                             cwAnn: CodewordANN,
                             parAnn: ParANN)
      extends VQH

  def apply(cns: HashMap[VQH#Codeword, Float],
            latest: VQH#Codeword,
            ntot: Float,
            k: Int,
            rng: IRng,
            cwAnn: CodewordANN,
            parAnn: ParANN): VQH = VQHImpl(cns, latest, ntot, k, rng, cwAnn, parAnn)

  def empty(dims: List[Int], k: Int): VQH = {
    val l = math.round(math.sqrt(k.toDouble)).toInt
    val init = dims.map(dim => Nd4j.zeros(1l, dim))
    val rng1 = IRng(k.hashCode)
    val (cwAnn, rng2) = CodewordANN.empty(l, dims, rng1)
    val (parAnn, rng3) = ParANN.empty(l, dims, rng2)
    apply(HashMap.empty[VQH#Codeword, Float], init, 0, k, rng3, cwAnn, parAnn)
  }

}

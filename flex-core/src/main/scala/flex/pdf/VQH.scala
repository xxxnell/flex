package flex.pdf

import flex.nns.ANN.syntax._
import flex.nns._
import flex.pdf.Bernoulli.syntax._
import flex.rand._
import flex.vec._

import scala.collection.immutable.HashMap

trait VQH {

  /**
   * (codeword vectors -> counts)
   * */
  val cns: HashMap[SumVec, Float]

  /**
   * Latest codeword vector.
   * */
  val latest: SumVec

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
  val cwNns: SumVecANN

  /**
   * NNS index for partial codeword vectors.
   * */
  val parCwNns: ParVecANN

}

trait VQHOps {

  def patchCount(vqh: VQH, x: SumVec, n: Float): VQH = {
    val cns1 = vqh.cns.updated(x, n)
    val ntot1 = vqh.ntot - vqh.cns.getOrElse(x, 0f) + n
    VQH(cns1, vqh.latest, ntot1, vqh.k, vqh.rng, vqh.cwNns, vqh.parCwNns)
  }

  def patchRng(vqh: VQH, rng: IRng): VQH =
    VQH(vqh.cns, vqh.latest, vqh.ntot, vqh.k, rng, vqh.cwNns, vqh.parCwNns)

  def add(vqh: VQH, x: SumVec, n: Float): VQH = {
    val cns1 = vqh.cns.+((x, n))
    val cwAnn1 = vqh.cwNns.add(x)
    val parAnn1 = vqh.parCwNns.add(x)
    VQH(cns1, x, vqh.ntot + n, vqh.k, vqh.rng, cwAnn1, parAnn1)
  }

  def remove(vqh: VQH, x: SumVec): VQH = {
    val cns1 = vqh.cns.-(x)
    val cwAnn1 = vqh.cwNns.remove(x)
    val parAnn1 = vqh.parCwNns.remove(x)
    VQH(cns1, vqh.latest, vqh.ntot - vqh.cns.getOrElse(x, 0f), vqh.k, vqh.rng, cwAnn1, parAnn1)
  }

  /**
   * Update partial input vectors.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def parUpdate(vqh: VQH, xps: List[(Vec, Int, Float)]): (VQH, List[SumVec], List[SumVec]) =
    xps.foldLeft((vqh, List.empty[SumVec], List.empty[SumVec])) {
      case ((_vqh, _cnews, _couts), (xp, a, w)) =>
        val cnew: SumVec => SumVec = c => diffusionExcept(c.updated(a, xp), a :: Nil)
        val (vqh1, cnews1, couts1) = parSearch(vqh, xp, a)
          .map(_c => singleUpdate(_vqh, _c, cnew(_c), w))
          .getOrElse((add(_vqh, cnew(vqh.latest), w), cnew(vqh.latest) :: Nil, Nil))
        (vqh1, cnews1 ++ _cnews, couts1 ++ _couts)
    }

  /**
   * Update input vectors.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def expUpdate(vqh: VQH, xs: List[(SumVec, Float)]): (VQH, List[SumVec], List[SumVec]) =
    xs.foldLeft((vqh, List.empty[SumVec], List.empty[SumVec])) {
      case ((_vqh, _newcs, _couts), (x, w)) =>
        val (vqh1, cnews1, couts1) = expSearch(vqh, x)
          .map(c => singleUpdate(_vqh, c, x, w))
          .getOrElse((add(_vqh, x, w), x :: Nil, Nil))
        (vqh1, cnews1 ++ _newcs, couts1 ++ _couts)
    }

  def singleUpdate(vqh: VQH, c: SumVec, cnew: => SumVec, w: Float): (VQH, List[SumVec], List[SumVec]) = {
    // Step A. Increase the count
    val n = vqh.cns.getOrElse(c, 0f) + w
    val vqh1 = patchCount(vqh, c, n)

    // Step B. Add a new codeword vector
    val avgpi = 1 / vqh.k.toFloat
    val (berp, p) = Bernoulli(sigmoid(n / vqh.ntot - avgpi), vqh.rng).sample
    val (vqh2, cnews) = if (p == 1) {
      val _vqh = patchCount(vqh1, c, n / 2)
      (add(_vqh, cnew, n / 2), cnew :: Nil)
      // TODO abruptly forget loop
    } else (vqh1, Nil)

    // Step C. Remove old codeword vectors
    val qtot = vqh2.cns.map { case (_, _n) => sigmoid(avgpi - _n / vqh2.ntot) }.sum
    val (vqh3, berq, couts) = vqh2.cns.foldLeft(vqh2, berp, List.empty[SumVec]) {
      case ((_vqh0, _berq0, _couts0), (_c, _n)) =>
        val (_berq1, q) = Bernoulli(sigmoid(avgpi - _n / vqh2.ntot) * avgpi, _berq0.rng).sample
        val (_vqh1, _couts1) = if (q == 1) (remove(_vqh0, _c), _c :: _couts0) else (_vqh0, _couts0)
        (_vqh1, _berq1, _couts1)
    }

    (patchRng(vqh3, berq.rng), cnews, couts)
  }

  def parSearch(vqh: VQH, xp: Vec, i: Int): Option[SumVec] = vqh.parCwNns.search(xp, i)

  def expSearch(vqh: VQH, x: SumVec): Option[SumVec] = vqh.cwNns.search(x)

  def size(vqh: VQH): Int = vqh.cns.size

  private def sigmoid(x: Float): Float = 1 / (1 + math.exp(-1 * x).toFloat)

  // TODO
  private def diffusion(x: Vec): Vec = x

  private def diffusionExcept(x: SumVec, is: List[Int]): SumVec = x.zipWithIndex.map {
    case (cp, b) => if (is.contains(b)) cp else diffusion(cp)
  }

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def add(x: SumVec, n: Float): VQH = VQH.add(vqh, x, n)
    def remove(x: SumVec): VQH = VQH.remove(vqh, x)
    def parUpdate(xps: List[(Vec, Int, Float)]): (VQH, List[SumVec], List[SumVec]) =
      VQH.parUpdate(vqh, xps)
    def expUpdate(xs: List[(SumVec, Float)]): (VQH, List[SumVec], List[SumVec]) =
      VQH.expUpdate(vqh, xs)
    def parSearch(xp: Vec, i: Int): Option[SumVec] = VQH.parSearch(vqh, xp, i)
    def expSearch(x: SumVec): Option[SumVec] = VQH.expSearch(vqh, x)
    def size: Int = VQH.size(vqh)
  }

}

object VQH extends VQHOps {

  object syntax extends VQHSyntax

  private case class VQHImpl(cns: HashMap[SumVec, Float],
                             latest: SumVec,
                             ntot: Float,
                             k: Int,
                             rng: IRng,
                             cwNns: SumVecANN,
                             parCwNns: ParVecANN)
      extends VQH

  def apply(cns: HashMap[SumVec, Float],
            latest: SumVec,
            ntot: Float,
            k: Int,
            rng: IRng,
            cwNns: SumVecANN,
            parCwNns: ParVecANN): VQH = VQHImpl(cns, latest, ntot, k, rng, cwNns, parCwNns)

  def empty(dims: List[Int], k: Int): VQH = {
    val l = math.round(math.sqrt(k.toDouble)).toInt
    val init = SumVec.zeros(dims)
    val rng1 = IRng(k.hashCode)
    val (cwAnn, rng2) = SumVecANN.empty(l, dims, rng1)
    val (parAnn, rng3) = ParVecANN.empty(l, dims, rng2)
    apply(HashMap.empty[SumVec, Float], init, 0, k, rng3, cwAnn, parAnn)
  }

}

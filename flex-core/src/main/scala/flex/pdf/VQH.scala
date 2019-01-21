package flex.pdf

import flex.nns._
import flex.nns.syntax._
import flex.pdf.syntax._
import flex.pdf.Bernoulli.syntax._
import flex.rand._
import flex.vec._

import scala.collection.immutable.HashMap

trait VQH {

  /**
   * (codeword vectors -> counts)
   * */
  val cwns: HashMap[SumVec, Float]

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

  override def toString: String = {
    val dims = VQH.dims(this)
    val dimsStr = if (dims.isEmpty) "0" else dims.mkString("+")
    s"VQH(k: $k, ntot: $ntot, dim: $dimsStr, size: ${VQH.size(this)})"
  }

}

trait VQHOps {

  def patchCount(vqh: VQH, x: SumVec, n: Float): VQH = {
    val cwns1 = vqh.cwns.updated(x, n)
    val ntot1 = vqh.ntot - vqh.cwns.getOrElse(x, 0f) + n
    VQH(cwns1, vqh.latest, ntot1, vqh.k, vqh.rng, vqh.cwNns, vqh.parCwNns)
  }

  def patchRng(vqh: VQH, rng: IRng): VQH =
    VQH(vqh.cwns, vqh.latest, vqh.ntot, vqh.k, rng, vqh.cwNns, vqh.parCwNns)

  def patchK(vqh: VQH, k: Int): VQH =
    VQH(vqh.cwns, vqh.latest, vqh.ntot, k, vqh.rng, vqh.cwNns, vqh.parCwNns)

  def renewNns(vqh: VQH): VQH = {
    val cws = vqh.cwns.keySet.toList
    val l = vqh.cwNns.lshs.size
    val dims = cws.headOption.map(cw => cw.dims).getOrElse(vqh.cwNns.dims)
    val (cwNns1, rng1) = SumVecANN.empty(l, dims, vqh.rng)
    val cwNns2 = cwNns1.adds(cws)
    val (parCwNns1, rng2) = ParVecANN.empty(l, dims, rng1)
    val parCwNns2 = parCwNns1.adds(cws)
    VQH(vqh.cwns, vqh.latest, vqh.ntot, vqh.k, rng2, cwNns2, parCwNns2)
  }

  /**
   * Add cordword vector.
   * */
  def addCw(vqh: VQH, x: SumVec, n: Float): VQH = {
    val cwns1 = vqh.cwns.+((x, n))
    val cwAnn1 = vqh.cwNns.add(x)
    val parAnn1 = vqh.parCwNns.add(x)
    VQH(cwns1, x, vqh.ntot + n, vqh.k, vqh.rng, cwAnn1, parAnn1)
  }

  /**
   * Remove codeword vector.
   * */
  def removeCw(vqh: VQH, x: SumVec): VQH = {
    val cwns1 = vqh.cwns.-(x)
    val cwAnn1 = vqh.cwNns.remove(x)
    val parAnn1 = vqh.parCwNns.remove(x)
    VQH(cwns1, vqh.latest, vqh.ntot - vqh.cwns.getOrElse(x, 0f), vqh.k, vqh.rng, cwAnn1, parAnn1)
  }

  def addDim(vqh: VQH, priors: List[Dist[Double]]): VQH = {
    def samples(ps: List[Dist[Double]], rng: IRng): (List[Double], IRng) = ps.foldRight((List.empty[Double], rng)) {
      case (p1, (ss, _rng)) =>
        val (p2, s) = p1.modifyRng(_ => _rng).sample
        (s :: ss, p2.rng)
    }
    val (adds, rng1) = (0 until vqh.cwns.size).foldRight((List.empty[Vec], vqh.rng)) {
      case (_, (vecs, _rng0)) =>
        val (ss, _rng1) = samples(priors, _rng0)
        (Vec(ss) :: vecs, _rng1)
    }
    val zipped = vqh.cwns.zip(adds)

    // cwns
    val cwns1 = zipped.map { case ((cw, n), add) => (cw.:+(add), n) }

    // latest
    val latest1rng1 = zipped.find(_._1._1 == vqh.latest).map { case ((cw, n), add) => (cw.:+(add), rng1) }
    lazy val (latss, rng2) = samples(priors, rng1)
    val (latest2, rng3) = latest1rng1.getOrElse((vqh.latest.:+(Vec(latss)), rng2))

    renewNns(VQH(cwns1, latest2, vqh.ntot, vqh.k, rng3, vqh.cwNns, vqh.parCwNns))
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
          .getOrElse((addCw(_vqh, cnew(vqh.latest), w), cnew(vqh.latest) :: Nil, Nil))
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
          .getOrElse((addCw(_vqh, x, w), x :: Nil, Nil))
        (vqh1, cnews1 ++ _newcs, couts1 ++ _couts)
    }

  def singleUpdate(vqh: VQH, c: SumVec, cnew: => SumVec, w: Float): (VQH, List[SumVec], List[SumVec]) = {
    // Step A. Increase the count
    val n = vqh.cwns.getOrElse(c, 0f) + w
    val vqh1 = patchCount(vqh, c, n)

    // Step B. Add a new codeword vector
    val avgpi = 1 / vqh.k.toFloat
    val (berp, p) = Bernoulli(sigmoid(n / vqh.ntot - avgpi), vqh.rng).sample
    val (vqh2, cnews) = if (p == 1) {
      val _vqh = patchCount(vqh1, c, n / 2)
      (addCw(_vqh, cnew, n / 2), cnew :: Nil)
      // TODO abruptly forget loop
    } else (vqh1, Nil)

    // Step C. Remove old codeword vectors
    val qtot = vqh2.cwns.map { case (_, _n) => sigmoid(avgpi - _n / vqh2.ntot) }.sum
    val (vqh3, berq, couts) = vqh2.cwns.foldLeft(vqh2, berp, List.empty[SumVec]) {
      case ((_vqh0, _berq0, _couts0), (_c, _n)) =>
        val (_berq1, q) = Bernoulli(sigmoid(avgpi - _n / vqh2.ntot) * avgpi, _berq0.rng).sample
        val (_vqh1, _couts1) = if (q == 1) (removeCw(_vqh0, _c), _c :: _couts0) else (_vqh0, _couts0)
        (_vqh1, _berq1, _couts1)
    }

    (patchRng(vqh3, berq.rng), cnews, couts)
  }

  def parSearch(vqh: VQH, xp: Vec, i: Int): Option[SumVec] = vqh.parCwNns.search(xp, i)

  def expSearch(vqh: VQH, x: SumVec): Option[SumVec] = vqh.cwNns.search(x)

  def size(vqh: VQH): Int = vqh.cwns.size

  def dim(vqh: VQH): Int = vqh.latest.dim

  def dims(vqh: VQH): List[Int] = vqh.latest.dims

  // TODO
  private def diffusion(x: Vec): Vec = x

  private def diffusionExcept(x: SumVec, is: List[Int]): SumVec = x.zipWithIndex.map {
    case (cp, b) => if (is.contains(b)) cp else diffusion(cp)
  }

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def add(x: SumVec, n: Float): VQH = VQH.addCw(vqh, x, n)
    def remove(x: SumVec): VQH = VQH.removeCw(vqh, x)
    def addDim(priors: List[Dist[Double]]): VQH = VQH.addDim(vqh, priors)
    def parUpdate(xps: List[(Vec, Int, Float)]): (VQH, List[SumVec], List[SumVec]) =
      VQH.parUpdate(vqh, xps)
    def expUpdate(xs: List[(SumVec, Float)]): (VQH, List[SumVec], List[SumVec]) =
      VQH.expUpdate(vqh, xs)
    def expUpdateTrace(xs: List[(SumVec, Float)]): List[VQH] =
      xs.foldLeft(Vector(vqh)) { case (vqhs, x) => vqhs.:+(VQH.expUpdate(vqhs.last, x :: Nil)._1) }.toList
    def parSearch(xp: Vec, i: Int): Option[SumVec] = VQH.parSearch(vqh, xp, i)
    def expSearch(x: SumVec): Option[SumVec] = VQH.expSearch(vqh, x)
    def size: Int = VQH.size(vqh)
    def dim: Int = VQH.dim(vqh)
    def dims: List[Int] = VQH.dims(vqh)
  }

}

object VQH extends VQHOps {

  object syntax extends VQHSyntax

  private case class VQHImpl(cwns: HashMap[SumVec, Float],
                             latest: SumVec,
                             ntot: Float,
                             k: Int,
                             rng: IRng,
                             cwNns: SumVecANN,
                             parCwNns: ParVecANN)
      extends VQH

  def apply(cwns: HashMap[SumVec, Float],
            latest: SumVec,
            ntot: Float,
            k: Int,
            rng: IRng,
            cwNns: SumVecANN,
            parCwNns: ParVecANN): VQH = VQHImpl(cwns, latest, ntot, k, rng, cwNns, parCwNns)

  def empty(dims: List[Int], k: Int): VQH = {
    val l = math.round(math.sqrt(k.toDouble)).toInt
    val init = SumVec.zeros(dims)
    val rng1 = IRng(k.hashCode)
    val (cwAnn, rng2) = SumVecANN.empty(l, dims, rng1)
    val (parAnn, rng3) = ParVecANN.empty(l, dims, rng2)
    apply(HashMap.empty[SumVec, Float], init, 0, k, rng3, cwAnn, parAnn)
  }

}

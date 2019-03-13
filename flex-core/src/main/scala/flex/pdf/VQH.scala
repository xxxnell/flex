package flex.pdf

import cats.implicits._
import flex.nns._
import flex.nns.syntax._
import flex.pdf.Bernoulli.syntax._
import flex.pdf.syntax._
import flex.rand._
import flex.util.IdentityHashMap.syntax._
import flex.util.RandomIdentitySet.syntax._
import flex.util.{IdentityHashMap, RandomIdentitySet}
import flex.vec._

trait VQH {

  /**
   * Codeword vectors.
   * */
  val cws: RandomIdentitySet[SumVec]

  /**
   * Counts w.r.t. codeword vectors.
   * */
  val ns: IdentityHashMap[SumVec, Float]

  /**
   * Last changed codeword vector.
   * */
  val last: SumVec

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
  val nns: SumVecANN

  /**
   * NNS index for partial codeword vectors.
   * */
  val parnns: ParVecANN

  override def toString: String = {
    def dimsStr(dims: List[Int]): String = if (dims.isEmpty) "0" else dims.mkString("+")
    val vqhDims = VQH.dims(this)
    val parNnsDims = parnns.dims

    s"VQH(k: $k, ntot: $ntot, " +
      s"dim: ${dimsStr(vqhDims)}, size: ${VQH.size(this)}, " +
      s"nns.dim: ${nns.dim}, parnns.dim: ${dimsStr(parNnsDims)})"
  }

}

trait VQHOps extends VQHLaws { self =>

  def patchRng(vqh: VQH, rng: IRng): VQH =
    VQH(vqh.cws.patchRng(rng), vqh.ns, vqh.last, vqh.ntot, vqh.k, rng, vqh.nns, vqh.parnns)

  def patchK(vqh: VQH, k: Int): VQH =
    VQH(vqh.cws, vqh.ns, vqh.last, vqh.ntot, k, vqh.rng, vqh.nns, vqh.parnns)

  def l(k: Int): Int = {
    val base: Double = 10.0
    math.round(math.log(k.toDouble) / math.log(base)).toInt + 1
  }

  def cache(k: Int): Int = k * 2

  def renewNns(vqh: VQH): VQH = {
    val cws = vqh.cws.toList
    val l = self.l(vqh.k)
    val dims = vqh.last.dims
    val (cwNns1, rng1) = SumVecANN.empty(l, dims, cache(vqh.k), vqh.rng)
    val parCwNns1 = ParVecANN.fromSumVecANN(cwNns1)
    val cwNns2 = cwNns1.adds(cws)
    val parCwNns2 = parCwNns1.adds(cws)

    patchRng(VQH(vqh.cws, vqh.ns, vqh.last, vqh.ntot, vqh.k, vqh.rng, cwNns2, parCwNns2), rng1)
  }

  def addCw(vqh: VQH, x: SumVec, n: Float): VQH = {
    val n1 = vqh.ns.getOrElse(x, 0f) + n
    val ns1 = if (n1 >= 0) vqh.ns.updated(x, n1) else ???
    val ntot1 = vqh.ntot + n
    val (cws1, latest1, nns1, parnns1) =
      if (!vqh.cws.contains(x)) (vqh.cws.+(x), x, vqh.nns.add(x), vqh.parnns.add(x))
      else (vqh.cws, vqh.last, vqh.nns, vqh.parnns)

    VQH(cws1, ns1, latest1, ntot1, vqh.k, vqh.rng, nns1, parnns1)
  }

  def removeCw(vqh: VQH, x: SumVec): VQH = {
    val cws1 = vqh.cws.-(x)
    val ns1 = vqh.ns.-(x)
    val cwAnn1 = vqh.nns.remove(x)
    val parAnn1 = vqh.parnns.remove(x)

    VQH(cws1, ns1, vqh.last, vqh.ntot - vqh.ns.getOrElse(x, 0f), vqh.k, vqh.rng, cwAnn1, parAnn1)
  }

  def addDim(vqh: VQH, priors: List[Dist[Double]]): VQH = {
    val (adds, rng1) = (0 until vqh.cws.size).foldRight((List.empty[Vec], vqh.rng)) {
      case (_, (_adds, _rng0)) => randVec(priors, _rng0).leftMap(add => add :: _adds)
    }
    val tf = vqh.cws.toList.zip(adds).map { case (cw, add) => cw -> cw.:+(add) }.toMap

    val cws1 = RandomIdentitySet(vqh.cws.rng, vqh.cws.toList.map(cw => tf(cw)))
    val ns1 = vqh.ns.map { case (cw, n) => (tf(cw), n) }
    val (latest1, rng2) = tf.get(vqh.last).fold(randVec(priors, rng1).leftMap(vec => vqh.last.:+(vec)))((_, rng1))

    renewNns(patchRng(VQH(cws1, ns1, latest1, vqh.ntot, vqh.k, vqh.rng, vqh.nns, vqh.parnns), rng2))
  }

  private def randVec(priors: List[Dist[Double]], rng: IRng): (Vec, IRng) =
    priors
      .foldRight((List.empty[Double], rng)) {
        case (p1, (ss, _rng)) => p1.modifyRng(_ => _rng).sample.swap.bimap(_ :: ss, _.rng)
      }
      .leftMap(rnds => Vec(rnds))

  def clear(vqh: VQH): Unit = {
    vqh.nns.clear
    vqh.parnns.clear
  }

}

trait VQHLaws { self: VQHOps =>

  def size(vqh: VQH): Int = vqh.cws.size

  def dim(vqh: VQH): Int = vqh.last.dim

  def dims(vqh: VQH): List[Int] = vqh.last.dims

  /**
   * Update partial input vectors.
   * @return (Updated VQH, new codeword-vectors, out codeword-vectors)
   * */
  def parUpdate(vqh: VQH,
                xps: List[(Vec, Int, Float)],
                complete: (Vec, Int, SumVec) => SumVec): (VQH, List[SumVec], List[SumVec]) =
    xps.foldLeft((vqh, List.empty[SumVec], List.empty[SumVec])) {
      case ((_vqh, _cnews, _couts), (xp, a, w)) =>
        val (vqh1, cnews1, couts1) = parSearch(vqh, xp, a)
          .map(_c => singleUpdate(_vqh, _c, complete(xp, a, _c), w))
          .getOrElse((addCw(_vqh, vqh.last, w), vqh.last :: Nil, Nil))
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
    val vqh1 = addCw(vqh, c, w)

    // Step B. Add a new codeword vector
    val n0 = vqh.ns.getOrElse(c, 0f)
    val avgpi = 1 / vqh.k.toFloat
    val (rng1, p) = Bernoulli(sigmoid((n0 + w) / vqh.ntot - avgpi), vqh.rng).sample.leftMap(_.rng)
    val (vqh2, cnews) = if (p == 1) {
      val _vqh1 = addCw(vqh1, c, -1 * (n0 + w) / 2)
      val _vqh2 = addCw(_vqh1, cnew, (n0 + w) / 2)
      (_vqh2, cnew :: Nil)
      // TODO abruptly forget loop
    } else (vqh1, Nil)

    // Step C. Remove old codeword vectors
    val (vqh3, rng2, couts) = vqh2.ns.toMap.foldLeft(vqh2, rng1, List.empty[SumVec]) {
      case ((_vqh0, _rng0, _couts0), (_c, _n)) =>
        val (_rng1, q) = Bernoulli(sigmoid(avgpi - _n / vqh2.ntot) * avgpi, _rng0).sample.leftMap(_.rng)
        val (_vqh1, _couts1) = if (q == 1) (removeCw(_vqh0, _c), _c :: _couts0) else (_vqh0, _couts0)
        (_vqh1, _rng1, _couts1)
    }

    (patchRng(vqh3, rng2), cnews, couts)
  }

  def parSearch(vqh: VQH, xp: Vec, i: Int): Option[SumVec] = vqh.parnns.search(xp, i)

  def expSearch(vqh: VQH, x: SumVec): Option[SumVec] = vqh.nns.search(x)

  def rand(vqh: VQH): (VQH, SumVec) = vqh.cws.rand.bimap(cws => patchRng(vqh, cws.rng), sv => sv.getOrElse(vqh.last))

  def addStd(vqh: VQH, dims: List[Int]): VQH = dims.foldLeft(vqh) {
    case (_vqh, dim) => addDim(_vqh, List.fill(dim)(NormalDist.std))
  }

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def size: Int = VQH.size(vqh)
    def dim: Int = VQH.dim(vqh)
    def dims: List[Int] = VQH.dims(vqh)
    def add(x: SumVec, n: Float): VQH = VQH.addCw(vqh, x, n)
    def remove(x: SumVec): VQH = VQH.removeCw(vqh, x)
    def addDim(priors: List[Dist[Double]]): VQH = VQH.addDim(vqh, priors)
    def parUpdate(xps: List[(Vec, Int, Float)],
                  complete: (Vec, Int, SumVec) => SumVec): (VQH, List[SumVec], List[SumVec]) =
      VQH.parUpdate(vqh, xps, complete)
    def expUpdate(xs: List[(SumVec, Float)]): (VQH, List[SumVec], List[SumVec]) = VQH.expUpdate(vqh, xs)
    def expUpdateTrace(xs: List[(SumVec, Float)]): List[VQH] =
      xs.foldLeft(Vector(vqh)) { case (vqhs, x) => vqhs.:+(VQH.expUpdate(vqhs.last, x :: Nil)._1) }.toList
    def parSearch(xp: Vec, i: Int): Option[SumVec] = VQH.parSearch(vqh, xp, i)
    def expSearch(x: SumVec): Option[SumVec] = VQH.expSearch(vqh, x)
    def rand: (VQH, SumVec) = VQH.rand(vqh)
    def addStd(dims: List[Int]): VQH = VQH.addStd(vqh, dims)
    def clear: Unit = VQH.clear(vqh)
  }

}

object VQH extends VQHOps { self =>

  object syntax extends VQHSyntax

  private case class VQHImpl(cws: RandomIdentitySet[SumVec],
                             ns: IdentityHashMap[SumVec, Float],
                             last: SumVec,
                             ntot: Float,
                             k: Int,
                             rng: IRng,
                             nns: SumVecANN,
                             parnns: ParVecANN)
      extends VQH

  def apply(cws: RandomIdentitySet[SumVec],
            ns: IdentityHashMap[SumVec, Float],
            latest: SumVec,
            ntot: Float,
            k: Int,
            rng: IRng,
            cwNns: SumVecANN,
            parCwNns: ParVecANN): VQH = VQHImpl(cws, ns, latest, ntot, k, rng, cwNns, parCwNns)

  def empty(dims: List[Int], k: Int): VQH = {
    val l = self.l(k)
    val init = SumVec.zeros(dims)
    val rng1 = IRng(k.hashCode)
    val (cwAnn, rng2) = SumVecANN.empty(l, dims, cache(k), rng1)
    val (parAnn, rng3) = ParVecANN.empty(l, dims, cache(k), rng2)

    apply(RandomIdentitySet.empty[SumVec](rng3), IdentityHashMap.empty[SumVec, Float], init, 0, k, rng3, cwAnn, parAnn)
  }

  def empty(dim: Int, k: Int): VQH = empty(dim :: Nil, k)

}

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
import org.nd4j.linalg.factory.Nd4j

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

  /**
   * @param rand Random vector generator w.r.t a prior distribution
   * */
  def addDim(vqh: VQH, rand: IRng => (SumVec, IRng)): VQH = {
    val (adds, rng1) = (0 until vqh.cws.size).foldRight((List.empty[SumVec], vqh.rng)) {
      case (_, (_adds, _rng0)) => rand(_rng0).leftMap(add => add :: _adds)
    }
    val tf = IdentityHashMap(vqh.cws.toList.zip(adds).map { case (cw, add) => cw -> (cw ++ add) })

    val cws1 = RandomIdentitySet(vqh.cws.toList.flatMap(cw => tf.get(cw)), vqh.cws.rng)
    val ns1 = vqh.ns.flatMap { case (cw, n) => tf.get(cw).map(cw1 => (cw1, n)) }
    val (latest1, rng2) = tf.get(vqh.last).fold(rand(rng1).leftMap(sv => vqh.last ++ sv))((_, rng1))

    renewNns(patchRng(VQH(cws1, ns1, latest1, vqh.ntot, vqh.k, vqh.rng, vqh.nns, vqh.parnns), rng2))
  }

  def unzip(vqh: VQH): List[VQH] = {
    val h = vqh.last.size
    val rng = vqh.rng
    val ntot = vqh.ntot
    val k = vqh.k
    val cwss = vqh.cws.toList
      .foldRight(List.fill(h)(List.empty[SumVec])) {
        case (sv, accs) => accs.zip(sv).map { case (acc, v) => SumVec(v) :: acc }
      }
      .map(cws => RandomIdentitySet(cws, rng))
    val nss = vqh.ns.toMap.foldRight(List.fill(h)(IdentityHashMap.empty[SumVec, Float])) {
      case ((sv, n), accs) => accs.zip(sv).map { case (ns, v) => ns.add(SumVec(v) -> n) }
    }
    val lasts = vqh.last.map(vec => SumVec(vec))
    val nnss = vqh.nns.unzip.map(vann => SumVecANN.fromVecANN(vann))
    val paranns = vqh.parnns.unzip

    cwss.zip(nss).zip(lasts).zip(nnss).zip(paranns).map {
      case ((((cws, ns), last), nns), parann) => VQH(cws, ns, last, ntot, k, rng, nns, parann)
    }
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
          .getOrElse((addCw(_vqh, complete(xp, a, vqh.last), w), complete(xp, a, vqh.last) :: Nil, Nil))

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
    val n1 = vqh1.ns.getOrElse(c, 0f)
    val (pi, avgpi) = (n1 / vqh1.ntot, 1 / vqh1.k.toFloat)
    val (rng1, p) = Bernoulli(sigmoid(pi - avgpi), vqh1.rng).sample.leftMap(_.rng)
    val (vqh2, cnews) = if (p == 1) {
      val _vqh1 = addCw(vqh1, c, -1 * n1 / 2)
      val _vqh2 = addCw(_vqh1, cnew, n1 / 2)
      (_vqh2, cnew :: Nil)
      // TODO abruptly forget loop
    } else (vqh1, Nil)

    // Step C. Remove old codeword vectors
    val (vqh3, rng2, couts) = vqh2.ns.toMap.foldLeft(vqh2, rng1, List.empty[SumVec]) {
      case ((_vqh0, _rng0, _couts0), (_c, _n)) =>
        val pi = _n / vqh2.ntot
        val (_rng1, q) = Bernoulli(sigmoid(avgpi - pi) * avgpi, _rng0).sample.leftMap(_.rng)
        val (_vqh1, _couts1) = if (q == 1) (removeCw(_vqh0, _c), _c :: _couts0) else (_vqh0, _couts0)
        (_vqh1, _rng1, _couts1)
    }

    (patchRng(vqh3, rng2), cnews, couts)
  }

  def parSearch(vqh: VQH, xp: Vec, i: Int): Option[SumVec] = vqh.parnns.search(xp, i)

  def expSearch(vqh: VQH, x: SumVec): Option[SumVec] = vqh.nns.search(x)

  def rand(vqh: VQH): (VQH, SumVec) = vqh.cws.rand.bimap(cws => patchRng(vqh, cws.rng), sv => sv.getOrElse(vqh.last))

  def addDimStd(vqh: VQH, dims: List[Int]): VQH = addDim(vqh, rng => SumVec.std(dims, rng))

  def addCwNormal(vqh: VQH, locs: List[Vec], scales: List[Vec]): VQH = {
    val (sv, rng1) = SumVec.normal(locs, scales, vqh.rng)
    addCw(patchRng(vqh, rng1), sv, 0f)
  }

  def initNormal(vqh: VQH, locs: List[Vec], scales: List[Vec]): VQH =
    (0 until vqh.k).foldLeft(vqh) { case (_vqh, _) => addCwNormal(_vqh, locs, scales) }

  def clear(vqh: VQH): Unit = {
    vqh.nns.clear
    vqh.parnns.clear
  }

}

trait VQHSyntax {

  implicit class VQHSyntaxImpl(vqh: VQH) {
    def patchK(k: Int): VQH = VQH.patchK(vqh, k)
    def size: Int = VQH.size(vqh)
    def dim: Int = VQH.dim(vqh)
    def dims: List[Int] = VQH.dims(vqh)
    def add(x: SumVec, n: Float): VQH = VQH.addCw(vqh, x, n)
    def remove(x: SumVec): VQH = VQH.removeCw(vqh, x)
    def addDim(rand: IRng => (SumVec, IRng)): VQH = VQH.addDim(vqh, rand)
    def parUpdate(xps: List[(Vec, Int, Float)],
                  complete: (Vec, Int, SumVec) => SumVec): (VQH, List[SumVec], List[SumVec]) =
      VQH.parUpdate(vqh, xps, complete)
    def expUpdate(xs: List[(SumVec, Float)]): (VQH, List[SumVec], List[SumVec]) = VQH.expUpdate(vqh, xs)
    def expUpdateTrace(xs: List[(SumVec, Float)]): List[VQH] =
      xs.foldLeft(Vector(vqh)) { case (vqhs, x) => vqhs.:+(VQH.expUpdate(vqhs.last, x :: Nil)._1) }.toList
    def parSearch(xp: Vec, i: Int): Option[SumVec] = VQH.parSearch(vqh, xp, i)
    def expSearch(x: SumVec): Option[SumVec] = VQH.expSearch(vqh, x)
    def rand: (VQH, SumVec) = VQH.rand(vqh)
    def addDimStd(dims: List[Int]): VQH = VQH.addDimStd(vqh, dims)
    def addDimStd(dims: Int*): VQH = VQH.addDimStd(vqh, dims.toList)
    def initNormal(locs: List[Vec], scales: List[Vec]): VQH = VQH.initNormal(vqh, locs, scales)
    def clear: Unit = VQH.clear(vqh)
    def unzip: List[VQH] = VQH.unzip(vqh)
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
            nns: SumVecANN,
            parnns: ParVecANN): VQH = VQHImpl(cws, ns, latest, ntot, k, rng, nns, parnns)

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

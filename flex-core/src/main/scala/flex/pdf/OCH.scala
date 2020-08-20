package flex.pdf

import cats.implicits._
import flex.nns._
import flex.nns.syntax._
import flex.pdf.Bernoulli.syntax._
import flex.pdf.syntax._
import flex.rand._
import flex.util.IdentityHashMap.syntax._
import flex.util.RandomIdentitySet.syntax._
import flex.util.{ IdentityHashMap, RandomIdentitySet }
import flex.vec._
import org.nd4j.linalg.factory.Nd4j

trait OCH {

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
    val ochDims = OCH.dims(this)
    val parNnsDims = parnns.dims

    s"OCH(k: $k, ntot: $ntot, " +
    s"dim: ${dimsStr(ochDims)}, size: ${OCH.size(this)}, " +
    s"nns.dim: ${nns.dim}, parnns.dim: ${dimsStr(parNnsDims)})"
  }

}

trait OCHOps extends OCHLaws { self =>

  def patchRng(och: OCH, rng: IRng): OCH =
    OCH(och.cws.patchRng(rng), och.ns, och.last, och.ntot, och.k, rng, och.nns, och.parnns)

  def patchK(och: OCH, k: Int): OCH =
    OCH(och.cws, och.ns, och.last, och.ntot, k, och.rng, och.nns, och.parnns)

  def l(k: Int): Int = {
    val base: Double = 10.0
    math.round(math.log(k.toDouble) / math.log(base)).toInt + 1
  }

  def cache(k: Int): Int = k * 2

  def renewNns(och: OCH): OCH = {
    val cws = och.cws.toList
    val l = self.l(och.k)
    val dims = och.last.dims
    val (cwNns1, rng1) = SumVecANN.empty(l, dims, cache(och.k), och.rng)
    val parCwNns1 = ParVecANN.fromSumVecANN(cwNns1)
    val cwNns2 = cwNns1.adds(cws)
    val parCwNns2 = parCwNns1.adds(cws)

    patchRng(OCH(och.cws, och.ns, och.last, och.ntot, och.k, och.rng, cwNns2, parCwNns2), rng1)
  }

  def addCw(och: OCH, x: SumVec, n: Float): OCH = {
    val n1 = och.ns.getOrElse(x, 0f) + n
    val ns1 = if (n1 >= 0) och.ns.updated(x, n1) else ???
    val ntot1 = och.ntot + n
    val (cws1, latest1, nns1, parnns1) =
      if (!och.cws.contains(x)) (och.cws.+(x), x, och.nns.add(x), och.parnns.add(x))
      else (och.cws, och.last, och.nns, och.parnns)

    OCH(cws1, ns1, latest1, ntot1, och.k, och.rng, nns1, parnns1)
  }

  def removeCw(och: OCH, x: SumVec): OCH = {
    val cws1 = och.cws.-(x)
    val ns1 = och.ns.-(x)
    val cwAnn1 = och.nns.remove(x)
    val parAnn1 = och.parnns.remove(x)

    OCH(cws1, ns1, och.last, och.ntot - och.ns.getOrElse(x, 0f), och.k, och.rng, cwAnn1, parAnn1)
  }

  /**
   * @param rand Random vector generator w.r.t a prior distribution
   * */
  def addDim(och: OCH, rand: IRng => (SumVec, IRng)): OCH = {
    val (adds, rng1) = (0 until och.cws.size).foldRight((List.empty[SumVec], och.rng)) {
      case (_, (_adds, _rng0)) => rand(_rng0).leftMap(add => add :: _adds)
    }
    val tf = IdentityHashMap(och.cws.toList.zip(adds).map { case (cw, add) => cw -> (cw ++ add) })

    val cws1 = RandomIdentitySet(och.cws.toList.flatMap(cw => tf.get(cw)), och.cws.rng)
    val ns1 = och.ns.flatMap { case (cw, n) => tf.get(cw).map(cw1 => (cw1, n)) }
    val (latest1, rng2) = tf.get(och.last).fold(rand(rng1).leftMap(sv => och.last ++ sv))((_, rng1))

    renewNns(patchRng(OCH(cws1, ns1, latest1, och.ntot, och.k, och.rng, och.nns, och.parnns), rng2))
  }

  def unzip(och: OCH): List[OCH] = {
    val h = och.last.size
    val rng = och.rng
    val ntot = och.ntot
    val k = och.k
    val cwss = och.cws.toList
      .foldRight(List.fill(h)(List.empty[SumVec])) {
        case (sv, accs) => accs.zip(sv).map { case (acc, v) => SumVec(v) :: acc }
      }
      .map(cws => RandomIdentitySet(cws, rng))
    val nss = och.ns.toMap.foldRight(List.fill(h)(IdentityHashMap.empty[SumVec, Float])) {
      case ((sv, n), accs) => accs.zip(sv).map { case (ns, v) => ns.add(SumVec(v) -> n) }
    }
    val lasts = och.last.map(vec => SumVec(vec))
    val nnss = och.nns.unzip.map(vann => SumVecANN.fromVecANN(vann))
    val paranns = och.parnns.unzip

    cwss.zip(nss).zip(lasts).zip(nnss).zip(paranns).map {
      case ((((cws, ns), last), nns), parann) => OCH(cws, ns, last, ntot, k, rng, nns, parann)
    }
  }

}

trait OCHLaws { self: OCHOps =>

  def size(och: OCH): Int = och.cws.size

  def dim(och: OCH): Int = och.last.dim

  def dims(och: OCH): List[Int] = och.last.dims

  /**
   * Update partial input vectors.
   * @return (Updated OCH, new codeword-vectors, out codeword-vectors)
   * */
  def parUpdate(
                 och: OCH,
                 xps: List[(Vec, Int, Float)],
                 complete: (Vec, Int, SumVec) => SumVec): (OCH, List[SumVec], List[SumVec]) =
    xps.foldLeft((och, List.empty[SumVec], List.empty[SumVec])) {
      case ((_och, _cnews, _couts), (xp, a, w)) =>
        val (och1, cnews1, couts1) = parSearch(och, xp, a)
          .map(_c => singleUpdate(_och, _c, complete(xp, a, _c), w))
          .getOrElse((addCw(_och, complete(xp, a, och.last), w), complete(xp, a, och.last) :: Nil, Nil))

        (och1, cnews1 ++ _cnews, couts1 ++ _couts)
    }

  /**
   * Update input vectors.
   * @return (Updated och, new codeword-vectors, out codeword-vectors)
   * */
  def expUpdate(och: OCH, xs: List[(SumVec, Float)]): (OCH, List[SumVec], List[SumVec]) =
    xs.foldLeft((och, List.empty[SumVec], List.empty[SumVec])) {
      case ((_och, _newcs, _couts), (x, w)) =>
        val (och1, cnews1, couts1) =
          expSearch(och, x).map(c => singleUpdate(_och, c, x, w)).getOrElse((addCw(_och, x, w), x :: Nil, Nil))

        (och1, cnews1 ++ _newcs, couts1 ++ _couts)
    }

  def singleUpdate(och: OCH, c: SumVec, cnew: => SumVec, w: Float): (OCH, List[SumVec], List[SumVec]) = {
    // Step A. Increase the count
    val och1 = addCw(och, c, w)

    // Step B. Add a new codeword vector
    val n1 = och1.ns.getOrElse(c, 0f)
    val (pi, avgpi) = (n1 / och1.ntot, 1 / och1.k.toFloat)
    val (rng1, p) = Bernoulli(sigmoid(pi - avgpi), och1.rng).sample.leftMap(_.rng)
    val (och2, cnews) = if (p == 1) {
      val _och1 = addCw(och1, c, -1 * n1 / 2)
      val _och2 = addCw(_och1, cnew, n1 / 2)
      (_och2, cnew :: Nil)
      // TODO abruptly forget loop
    } else (och1, Nil)

    // Step C. Remove old codeword vectors
    val (och3, rng2, couts) = och2.ns.toMap.foldLeft(och2, rng1, List.empty[SumVec]) {
      case ((_och0, _rng0, _couts0), (_c, _n)) =>
        val pi = _n / och2.ntot
        val (_rng1, q) = Bernoulli(sigmoid(avgpi - pi) * avgpi, _rng0).sample.leftMap(_.rng)
        val (_och1, _couts1) = if (q == 1) (removeCw(_och0, _c), _c :: _couts0) else (_och0, _couts0)
        (_och1, _rng1, _couts1)
    }

    (patchRng(och3, rng2), cnews, couts)
  }

  def parSearch(och: OCH, xp: Vec, i: Int): Option[SumVec] = och.parnns.search(xp, i)

  def expSearch(och: OCH, x: SumVec): Option[SumVec] = och.nns.search(x)

  def rand(och: OCH): (OCH, SumVec) = och.cws.rand.bimap(cws => patchRng(och, cws.rng), sv => sv.getOrElse(och.last))

  def addDimStd(och: OCH, dims: List[Int]): OCH = addDim(och, rng => SumVec.std(dims, rng))

  def addCwNormal(och: OCH, locs: List[Vec], scales: List[Vec]): OCH = {
    val (sv, rng1) = SumVec.normal(locs, scales, och.rng)
    addCw(patchRng(och, rng1), sv, 0f)
  }

  def initNormal(och: OCH, locs: List[Vec], scales: List[Vec]): OCH =
    (0 until och.k).foldLeft(och) { case (_och, _) => addCwNormal(_och, locs, scales) }

  def clear(och: OCH): Unit = {
    och.nns.clear
    och.parnns.clear
  }

}

trait OCHSyntax {

  implicit class OCHSyntaxImpl(och: OCH) {
    def patchK(k: Int): OCH = OCH.patchK(och, k)
    def size: Int = OCH.size(och)
    def dim: Int = OCH.dim(och)
    def dims: List[Int] = OCH.dims(och)
    def add(x: SumVec, n: Float): OCH = OCH.addCw(och, x, n)
    def remove(x: SumVec): OCH = OCH.removeCw(och, x)
    def addDim(rand: IRng => (SumVec, IRng)): OCH = OCH.addDim(och, rand)
    def parUpdate(
        xps: List[(Vec, Int, Float)],
        complete: (Vec, Int, SumVec) => SumVec): (OCH, List[SumVec], List[SumVec]) =
      OCH.parUpdate(och, xps, complete)
    def expUpdate(xs: List[(SumVec, Float)]): (OCH, List[SumVec], List[SumVec]) = OCH.expUpdate(och, xs)
    def expUpdateTrace(xs: List[(SumVec, Float)]): List[OCH] =
      xs.foldLeft(Vector(och)) { case (ochs, x) => ochs.:+(OCH.expUpdate(ochs.last, x :: Nil)._1) }.toList
    def parSearch(xp: Vec, i: Int): Option[SumVec] = OCH.parSearch(och, xp, i)
    def expSearch(x: SumVec): Option[SumVec] = OCH.expSearch(och, x)
    def rand: (OCH, SumVec) = OCH.rand(och)
    def addDimStd(dims: List[Int]): OCH = OCH.addDimStd(och, dims)
    def addDimStd(dims: Int*): OCH = OCH.addDimStd(och, dims.toList)
    def initNormal(locs: List[Vec], scales: List[Vec]): OCH = OCH.initNormal(och, locs, scales)
    def clear: Unit = OCH.clear(och)
    def unzip: List[OCH] = OCH.unzip(och)
  }

}

object OCH extends OCHOps { self =>

  object syntax extends OCHSyntax

  private case class OCHImpl(
      cws: RandomIdentitySet[SumVec],
      ns: IdentityHashMap[SumVec, Float],
      last: SumVec,
      ntot: Float,
      k: Int,
      rng: IRng,
      nns: SumVecANN,
      parnns: ParVecANN)
      extends OCH

  def apply(
      cws: RandomIdentitySet[SumVec],
      ns: IdentityHashMap[SumVec, Float],
      latest: SumVec,
      ntot: Float,
      k: Int,
      rng: IRng,
      nns: SumVecANN,
      parnns: ParVecANN): OCH = OCHImpl(cws, ns, latest, ntot, k, rng, nns, parnns)

  def empty(dims: List[Int], k: Int): OCH = {
    val l = self.l(k)
    val init = SumVec.zeros(dims)
    val rng1 = IRng(k.hashCode)
    val (cwAnn, rng2) = SumVecANN.empty(l, dims, cache(k), rng1)
    val (parAnn, rng3) = ParVecANN.empty(l, dims, cache(k), rng2)

    apply(RandomIdentitySet.empty[SumVec](rng3), IdentityHashMap.empty[SumVec, Float], init, 0, k, rng3, cwAnn, parAnn)
  }

  def empty(dim: Int, k: Int): OCH = empty(dim :: Nil, k)

}

package flex.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck
import flex.nns.LSH.syntax._
import flex.pdf.VQH.syntax._
import org.nd4j.linalg.factory.Nd4j
import org.scalactic._
import TripleEquals._
import flex.rand.IRng
import org.scalactic.Tolerance._

class VQHSpec extends Specification with ScalaCheck {

  "VQH" should {

    "construct" in {
      val (dims, k) = (List(1, 2, 3, 4, 5), 10)
      val vqh = VQH.empty(dims, k)

      val cond1 = vqh.cns.isEmpty
      val cond2 = vqh.ntot == 0
      val cond3 = vqh.k == k
      val cond4 = vqh.cwNns.lshs.forall(lsh => lsh.dim == dims.sum)
      val cond5 = vqh.parCwNns.arrAnns.zip(dims).forall { case (ann, dim) => ann.lshs.forall(_.dim == dim) }

      if (!cond1) ko(s"cns: ${vqh.cns}")
      else if (!cond2) ko(s"ntot: ${vqh.ntot}")
      else if (!cond3) ko(s"k: ${vqh.k}")
      else if (!cond4) ko(s"cwAnn: ${vqh.cwNns}")
      else if (!cond5) ko(s"parAnn: ${vqh.parCwNns}")
      else ok
    }

    "ops" in {

      "add" in {
        val (dims, k) = (List(1, 2, 3, 4, 5), 10)
        val vqh0 = VQH.empty(dims, k)
        val xs = (1 to 10).toList.map(_ => (dims.map(dim => Nd4j.randn(1, dim)), 1.0f))
        val vqh1 = xs.foldLeft(vqh0) { case (_vqh, (x, w)) => _vqh.add(x, w) }

        val cond1 = vqh1.size == xs.size
        val cond2 = vqh1.latest == xs.last._1

        if (!cond1) ko(s"vqh.size: ${vqh1.size}, xs.size: ${xs.size}")
        else if (!cond2) ko(s"vqh1.latest: ${vqh1.latest}")
        else ok
      }

      "remove" in {
        val (dims, k) = (List(1, 2, 3, 4, 5), 10)
        val vqh0 = VQH.empty(dims, k)
        val xs = (1 to 10).toList.map(_ => (dims.map(dim => Nd4j.randn(1, dim)), 1.0f))
        val vqh1 = xs.foldLeft(vqh0) { case (_vqh, (x, w)) => _vqh.add(x, w) }
        val vqh2 = xs.foldLeft(vqh1) { case (_vqh, (x, w)) => _vqh.remove(x) }

        val expected = xs.size - xs.size
        val cond1 = vqh2.size == expected

        if (!cond1) ko(s"vqh.size: ${vqh2.size}, expected: $expected")
        else ok
      }

      "parUpdate" in {

        "first" in {
          val (dims, k, n, rng) = (List(1, 2, 3, 4, 5), 20, 3, IRng(0))
          val vqh0 = VQH.empty(dims, k)
          val (is, _) = (1 to n).toList.foldLeft((List.empty[Int], rng)) {
            case ((_is, _rng), _) => ((_rng.next._2 * dims.size).floor.toInt :: _is, _rng.next._1)
          }
          val xps = is.map(i => (Nd4j.randn(1, dims(i)), i, 1.0f))
          val (vqh1, cins, couts) = vqh0.parUpdate(xps)

          val cond1 = vqh1.cns.size == xps.size
          val cond2 = vqh1.ntot === xps.map(_._3).sum +- 0.01f

          if (!cond1) ko(s"cns: ${vqh1.cns}, \ncins: $cins, \ncouts: $couts")
          else if (!cond2) ko(s"ntot: ${vqh1.ntot}")
          else ok
        }

        "k" in {
          val (dims, k, n, rng) = (List(1, 2, 3, 4, 5), 20, 300, IRng(0))
          val vqh0 = VQH.empty(dims, k)
          val (is, _) = (1 to n).toList.foldLeft((List.empty[Int], rng)) {
            case ((_is, _rng), _) => ((_rng.next._2 * dims.size).floor.toInt :: _is, _rng.next._1)
          }
          val xps = is.map(i => (Nd4j.randn(1, dims(i)), i, 1.0f))
          val vqh1 = xps.foldLeft(vqh0) { case (_vqh, (x, i, w)) => _vqh.parUpdate((x, i, w) :: Nil)._1 }

          val cond1 = vqh1.size === k +- (k * 0.3).round.toInt

          if (!cond1) ko(s"vqh.size: ${vqh1.size}, expected: ${vqh1.k}")
          else ok(s"vqh.size: ${vqh1.size}, expected: ${vqh1.k}")
        }

      }

      "expUpdate" in {

        "first" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 20)
          val vqh0 = VQH.empty(dims, k)
          val xs = (1 to 10).toList.map(_ => (dims.map(dim => Nd4j.randn(1, dim)), 1.0f))
          val (vqh1, cins, couts) = vqh0.expUpdate(xs)

          val cond1 = vqh1.cns.size == xs.size
          val cond2 = vqh1.ntot === xs.map(_._2).sum +- 0.01f

          if (!cond1) ko(s"cns: ${vqh1.cns}, \ncins: $cins, \ncouts: $couts")
          else if (!cond2) ko(s"ntot: ${vqh1.ntot}")
          else ok
        }

        "k" in {
          val (dims, k, n) = (List(1, 2, 3, 4, 5), 20, 300)
          val vqh0 = VQH.empty(dims, k)
          val xs = (1 to n).toList.map(_ => (dims.map(dim => Nd4j.randn(1, dim)), 1.0f))
          val vqh1 = xs.foldLeft(vqh0) { case (_vqh, (x, w)) => _vqh.expUpdate((x, w) :: Nil)._1 }

          val cond1 = vqh1.size === k +- (k * 0.3).round.toInt

          if (!cond1) ko(s"vqh.size: ${vqh1.size}, expected: ${vqh1.k}")
          else ok(s"vqh.size: ${vqh1.size}, expected: ${vqh1.k}")
        }

      }

      "parSearch" in {

        "empty" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 10)
          val vqh0 = VQH.empty(dims, k)
          val i = 0
          val xp = Nd4j.randn(1, dims.apply(i))
          val res = vqh0.parSearch(xp, i)

          val cond1 = res.isEmpty

          if (!cond1) ko(s"result: $res")
          else ok
        }

        "basic" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 10)
          val vqh0 = VQH.empty(dims, k)
          val i = 0
          val x = dims.map(dim => Nd4j.randn(1, dim))
          val vqh1 = vqh0.add(x, 1.0f)
          val res = vqh1.parSearch(x.apply(i), i)

          val cond1 = res.contains(x)

          if (!cond1) ko(s"result: $res")
          else ok
        }

      }

      "expSearch" in {

        "empty" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 10)
          val vqh0 = VQH.empty(dims, k)
          val x = dims.map(dim => Nd4j.randn(1, dim))
          val res = vqh0.expSearch(x)

          val cond1 = res.isEmpty

          if (!cond1) ko(s"result: $res")
          else ok
        }

        "basic" in {
          val (dims, k) = (List(1, 2, 3, 4, 5), 10)
          val vqh0 = VQH.empty(dims, k)
          val x = dims.map(dim => Nd4j.randn(1, dim))
          val vqh1 = vqh0.add(x, 1.0f)
          val res = vqh1.expSearch(x)

          val cond1 = res.contains(x)

          if (!cond1) ko(s"result: $res")
          else ok
        }

      }

    }

  }

}

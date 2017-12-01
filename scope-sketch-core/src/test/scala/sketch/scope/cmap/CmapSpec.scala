package sketch.scope.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.cmap.DividerCmapGen.dividerGen
//import sketch.scope.cmap.Cmap

/**
  * Licensed by Probe Technology, Inc.
  */
class CmapSpec extends Specification with ScalaCheck {

  "Cmap" should {

    "bin" in {
      "divider" in {
        implicit val dividerGen = CmapGen.dividerA

        prop { (divider: List[Double]) =>
          val cmap = DividerCmap(divider)
          val dividerSize = divider.size
          val check = (1 until dividerSize)
            .map(idx => (idx, cmap.bin(idx)))
            .map { case (idx, bin) =>
              bin(0) == divider(idx - 1) && bin(1) == divider(idx)
            }.forall(identity)
          if(check) ok else ko
        }.setArbitrary(dividerGen)
      }

      "uniform" in {
        todo
      }
    }

    "size" in {
      implicit val cmapGen = CmapGen.cmapA

      prop { (sizeCmap: (Int, Cmap) ) =>
        val (size, cmap) = sizeCmap
        if(size + 1 == cmap.size) ok else ko
      }.setArbitrary(cmapGen)
    }

    "range" in {

      "divider" in {
        implicit val dividerGen = CmapGen.dividerA

        prop { (divider: List[Double]) =>
          val cmap = DividerCmap(divider)
          val dividerSize = divider.size
          val check = (1 until dividerSize)
            .map(idx => (idx, cmap.range(idx)))
            .map { case (idx, range) =>
              range(0) == divider(idx - 1) && range((divider(idx) - divider(idx - 1)).toInt) == divider(idx)
            }.forall(identity)
          if(check) ok else ko
        }.setArbitrary(dividerGen)
      }

      "uniform" in {
        todo
      }
    }

  }

}

object CmapGen {
  
  def cmapGen: Gen[(Int, Cmap)] = for {
    n <- Gen.choose(1, 10)
  } yield (n, Cmap.uniform(n))

  def dividerGen: Gen[List[Double]] = for {
    from <- Gen.choose(0, 100)
    to <- Gen.choose(0, 100)
    if from < to
    list = (from to to).toList.map(a => a.toDouble)
  } yield list

  def cmapA: Arbitrary[(Int, Cmap)] = Arbitrary(cmapGen)

  def dividerA: Arbitrary[List[Double]] = Arbitrary(dividerGen)

}
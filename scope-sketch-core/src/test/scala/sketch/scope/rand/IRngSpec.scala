package sketch.scope.rand

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable._

class IRngSpec extends Specification with ScalaCheck {

  "IRng" should {

    "next" in {

      "basic" in {
        implicit val iRngA: Arbitrary[IRng] = IRngGen.iRngA

        prop { (rng: IRng) =>
          rng.next
          ok
        }.setArbitrary(iRngA)
      }

      "variation" in {
        implicit val iRngA: Arbitrary[IRng] = IRngGen.iRngA

        prop { (rng0: IRng) =>
          val (rng1, rand1) = rng0.next
          val (rng2, rand2) = rng1.next
          if(rand1 != rand2) ok else ko
        }.setArbitrary(iRngA)
      }

      "consistency" in {
        implicit val iRngA: Arbitrary[IRng] = IRngGen.iRngA

        prop { (rng: IRng) =>
          val (rng1, rand1) = rng.next
          val (rng2, rand2) = rng.next
          if(rand1 == rand2) ok else ko(s"error: ${((rand1 - rand2) / rand1) * 100} %")
        }.setArbitrary(iRngA)
      }

    }

  }

}

object IRngGen {

  def iRngGen: Gen[IRng] = for {
    intSeed <- Arbitrary.arbitrary[Int]
    rng = IRng.apply(intSeed)
  } yield rng

  def iRngA: Arbitrary[IRng] = Arbitrary(iRngGen)

}
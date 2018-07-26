package flip.hcounter

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable._

class HCounterSpec extends Specification with ScalaCheck {

  "HCounter" should {

    "ops" in {

      "update" in {

        "basic" in {
          todo
        }

        "irregular index" in todo

      }

      "get" in {

        "basic" in {
          val index = 10
          val count = 20
          val hcounter0 = HCounter.empty(1, 50, 0)
          val hcounter1 = hcounter0.update(index, count)
          val cond = hcounter1.get(index) == count

          if(!cond) ko(s"count: ${hcounter1.get(index)}, expected: $count") else ok
        }

        "irregular index: overflow" in {
//          val hcounter = HCounter.empty(1, 100000, 0)
//          hcounter.get(100000).fold(ok)(hc => ko("Returns something with irregular index."))

          todo
        }

        "irregular index: underflow" in {
//          val hcounter = HCounter.empty(1, 100000, 0)
//          hcounter.get(-1).fold(ok)(hc => ko("Returns something with irregular index."))

          todo
        }

      }

    }

  }

}

object HCounterGen {

  def hcounterGen: Gen[HCounter] = for {
    depth <- Gen.choose(1, 10)
    width <- Gen.choose(100, 10000)
    seed = 0
  } yield HCounter.empty(depth, width, seed)

  def hcounterA: Arbitrary[HCounter] = Arbitrary(hcounterGen)

}
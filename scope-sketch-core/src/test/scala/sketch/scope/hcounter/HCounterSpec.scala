package sketch.scope.hcounter

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.counter.CounterGen

/**
  * Licensed by Probe Technology, Inc.
  */
class HCounterSpec extends Specification with ScalaCheck {

  "HCounter" should {

    "ops" in {

      "update" in {
        implicit val hcounterGen = HCounterGen.hcounterA

        prop { (hcounter: HCounter) =>
          val updatedHcounter = hcounter.update(10,20).get
          val updatedCount = hcounter.sum + 20
          if( updatedHcounter.sum == updatedCount) ok else ko (
              s"updatedCounter: $updatedCount, " +
              s"updatedHcounter.sum: $updatedHcounter.sum"
          )
        }.setArbitrary(hcounterGen)
      }

      "get" in {
        implicit val hcounterGen = HCounterGen.hcounterA

        prop { (hcounter: HCounter) =>
          val updatedHcounter = hcounter.update(10,20).get
          val updatedGet = updatedHcounter.get(10).get
          if( updatedGet == 20) ok else ko (
            s"updatedGet: $updatedGet"
          )
        }.setArbitrary(hcounterGen)
      }

    }

  }

}

object HCounterGen {

  def hcounterGen: Gen[HCounter] = for {
    depth <- Gen.choose(1, 10)
    width <- Gen.choose(100, 10000)
  } yield HCounter.empty(depth, width)

  def hcounterA: Arbitrary[HCounter] = Arbitrary(hcounterGen)

}
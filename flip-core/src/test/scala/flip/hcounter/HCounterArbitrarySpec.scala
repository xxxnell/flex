package flip.hcounter

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable._

class HCounterArbitrarySpec extends Specification with ScalaCheck {

  "HCounter" should {

    "ops" in {

      "update" in {
        implicit val hcounterGen: Arbitrary[HCounter] = HCounterGen.hcounterA

        prop { (hcounter: HCounter) =>
          val updatedHcounter = hcounter.update(10, 20)
          val cond = updatedHcounter.sum == (hcounter.sum + 20)

          if (cond) ok else ko
        }.setArbitrary(hcounterGen)
      }

      "get" in {
        implicit val hcounterGen: Arbitrary[HCounter] = HCounterGen.hcounterA

        prop { (hcounter: HCounter) =>
          val updatedHcounter = hcounter.update(10, 20)
          val value = updatedHcounter.get(10)
          val cond = value == 20

          if(cond) ok else ko
        }.setArbitrary(hcounterGen)
      }

    }

  }

}
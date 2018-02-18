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
          (for {
            updatedHcounter <- hcounter.update(10, 20)
            test = updatedHcounter.sum == (hcounter.sum + 20)
          } yield test)
            .fold(ko)(test => if (test) ok else ko)
        }.setArbitrary(hcounterGen)
      }

      "get" in {
        implicit val hcounterGen: Arbitrary[HCounter] = HCounterGen.hcounterA

        prop { (hcounter: HCounter) =>
          (for {
            updatedHcounter <- hcounter.update(10, 20)
            value <- updatedHcounter.get(10)
          } yield value == 20)
            .fold(ko)(test => if(test) ok else ko )

        }.setArbitrary(hcounterGen)
      }

    }

  }

}
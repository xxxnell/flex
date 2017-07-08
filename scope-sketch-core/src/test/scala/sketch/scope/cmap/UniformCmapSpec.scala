package sketch.scope.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class UniformCmapSpec extends Specification with ScalaCheck {

  "UniformCmap" should {

    "divider" in {
      implicit val uniformCmapA = UniformCmapGen.uniformCmapA

      prop { (cmap: UniformCmap) =>
        ok
      }
    }

  }

}

object UniformCmapGen {

  def uniformCmapGen: Gen[UniformCmap] = for {
    n <- Gen.choose(0, Math.pow(2, 19).toInt)
  } yield UniformCmap(n)

  def uniformCmapA: Arbitrary[UniformCmap] = Arbitrary(uniformCmapGen)

}
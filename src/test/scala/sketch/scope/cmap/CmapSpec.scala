package sketch.scope.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class CmapSpec extends Specification with ScalaCheck {

  "Cmap" should {

    todo

  }

}

object CmapGen {

  def cmapGen: Gen[Cmap] = UniformCmapGen.uniformCmapGen

  def cmapA: Arbitrary[Cmap] = Arbitrary(cmapGen)

}
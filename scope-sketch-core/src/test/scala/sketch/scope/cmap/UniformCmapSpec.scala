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

      prop { (cmapTupple: (Int, UniformCmap) ) =>
        if( cmapTupple._2.divider.length == cmapTupple._1) ok
        else ko
      }.setArbitrary(uniformCmapA)
    }

  }

}

object UniformCmapGen {

  def uniformCmapGen: Gen[(Int, UniformCmap)] = for {
    n <- Gen.choose(0, Math.pow(2, 19).toInt)
  } yield (n, UniformCmap(n))

  def uniformCmapA: Arbitrary[(Int, UniformCmap)] = Arbitrary(uniformCmapGen)

}
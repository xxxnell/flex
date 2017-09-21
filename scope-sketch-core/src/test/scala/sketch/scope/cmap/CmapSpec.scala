package sketch.scope.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
//import sketch.scope.cmap.Cmap

/**
  * Licensed by Probe Technology, Inc.
  */
class CmapSpec extends Specification with ScalaCheck {

  "Cmap" should {

    "bin" in {
      todo
    }

    "size" in {
      implicit val cmapGen = CmapGen.cmapA

      prop { (cmapTupple: (Int, Cmap) ) =>
        if( cmapTupple._1+1 == cmapTupple._2.size) ok
        else ko
      }.setArbitrary(cmapGen)
    }

    "range" in {
      todo
    }

  }

}

object CmapGen {
  
  def cmapGen: Gen[(Int, Cmap)] = for {
    n <- Gen.choose(1, 10)
  } yield (n, Cmap.uniform(n))

  def cmapA: Arbitrary[(Int, Cmap)] = Arbitrary(cmapGen)

}
package flex.cmap

import org.scalacheck.Arbitrary
import org.specs2.mutable._
import org.specs2.ScalaCheck

class CmapArbitrarySpec extends Specification with ScalaCheck {

  "Cmap" should {

    "size" in {
      implicit val cmapGen: Arbitrary[(Int, Cmap)] = CmapGen.cmapA

      prop { (sizeCmap: (Int, Cmap) ) =>
        val (size, cmap) = sizeCmap
        if(size == cmap.size) ok else ko
      }.setArbitrary(cmapGen)
    }

  }

}
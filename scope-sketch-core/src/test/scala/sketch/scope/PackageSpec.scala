package sketch.scope

import org.specs2.mutable._
import org.specs2.ScalaCheck

class PackageSpec extends Specification with ScalaCheck {

  "Package" should {

    "basic sketch" in {
      Sketch.empty[Double] must beAnInstanceOf[Sketch[Double]]
    }

    "sketch with custom conf" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: SketchConf = SketchConf(cmapSize, cmapNo, cmapMin, cmapMax, counterSize, counterNo)

      Sketch.empty[Double] must beAnInstanceOf[Sketch[Double]]
    }

  }

}
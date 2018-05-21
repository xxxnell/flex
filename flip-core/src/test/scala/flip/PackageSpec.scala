package flip

import flip.pdf.AdaSelSketch
import org.specs2.ScalaCheck
import org.specs2.mutable._

class PackageSpec extends Specification with ScalaCheck {

  "Package" should {

    "basic sketch" in {
      Sketch.empty[Double] must beAnInstanceOf[Sketch[Double]]
    }

    "basic sketch type" in {
      Sketch.empty[Double] must beAnInstanceOf[AdaSelSketch[Double]]
    }

    "sketch with custom conf" in {
      val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(0.0), Some(10.0))
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: SketchConf = SketchConf(
        cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
        counterSize = counterSize, counterNo = counterNo
      )

      Sketch.empty[Double] must beAnInstanceOf[AdaSelSketch[Double]]
    }

  }

}
package flex.pdf.update

import flex.implicits._
import flex.pdf.Sketch
import org.specs2.mutable._
import org.specs2.ScalaCheck

class EqUpdateSpec extends Specification with ScalaCheck {

  "EqUpdate" should {

    "updateCmap" in {
      implicit val conf: SketchConf = SketchConf(
        cmapSize = 1000, cmapNo = 5, cmapStart = Some(0d), cmapEnd = Some(10d),
        counterSize = 500, counterNo = 2
      )
      val sketch0 = Sketch.empty[Double]
      val ps = (0.0 to 100.0 by 1).toList.map(p => (p, 1.0))

      EqUpdate.updateCmapForSketch(sketch0, ps)
      ok
    }

  }

}
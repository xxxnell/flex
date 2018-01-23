package flip.pdf.update

import flip.conf.CustomSketchConf
import flip.pdf.Sketch
import org.specs2.mutable._
import org.specs2.ScalaCheck

class EqualSpaceCdfUpdateSpec extends Specification with ScalaCheck {

  "EqualSpaceCdfUpdate" should {

    "updateCmap" in {
      implicit val conf: CustomSketchConf = CustomSketchConf(
        cmapSize = 1000, cmapNo = 5, cmapStart = Some(0d), cmapEnd = Some(10d),
        counterSize = 500, counterNo = 2
      )
      val sketch0 = Sketch.empty[Double]
      val ps = (0.0 to 100.0 by 1).toList.map(p => (p, 1.0))

      EqualSpaceCdfUpdate.updateCmap(sketch0, ps).fold(ko)(cmap => ok)
    }

  }

}
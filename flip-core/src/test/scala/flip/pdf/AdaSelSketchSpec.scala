package flip.pdf

import flip.conf.CustomAdaSelSketchConf
import flip.pdf.Buffer.syntax._

import org.specs2.mutable._
import org.specs2.ScalaCheck

class AdaSelSketchSpec extends Specification with ScalaCheck {

  "AdaSelSketch" should {

    "type invariance after update" in {
      implicit val conf: CustomAdaSelSketchConf = CustomAdaSelSketchConf(
        bufferSize = 10,
        cmapSize = 100, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
        counterSize = 20, counterNo = 2
      )
      val sketch0: Sketch[Double] = AdaSelSketch.empty[Double]
      val sketch1 = sketch0.update(1)

      if(!sketch1.isInstanceOf[AdaSelSketch[Double]]) ko
      else if(sketch1.asInstanceOf[AdaSelSketch[Double]].buffer.isEmpty) ko("Buffer is cleaned.")
      else ok
    }

  }

}
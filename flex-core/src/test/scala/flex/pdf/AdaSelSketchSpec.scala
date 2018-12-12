package flex.pdf

import flex.conf.pdf.CustomAdaSelSketchConf
import flex.pdf.Buffer.syntax._
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

    "diagnose" in {

      "ignore" in {
        val bufferSize = 50
        implicit val conf: CustomAdaSelSketchConf = CustomAdaSelSketchConf(
          cmapSize = 20, cmapNo = 2, cmapStart = Some(-3.0), cmapEnd = Some(3.0),
          counterSize = 20, counterNo = 2,
          bufferSize = bufferSize,
          startThreshold = Integer.MAX_VALUE,
          rebuildThreshold = 0.1
        )
        val underlying = NormalDist.std
        val (_, samples) = underlying.samples(bufferSize)
        val sketch0: AdaSelSketch[Double] = AdaSelSketch.empty[Double]
        val sketch1 = sketch0.updateInOrder(samples)
        val diagnose = AdaSelSketch.diagnose(sketch1.asInstanceOf[AdaSelSketch[Double]])

        if(diagnose) ko(s"diagnose: $diagnose") else ok
      }

      "shifted" in {
        val bufferSize = 50
        implicit val conf: CustomAdaSelSketchConf = CustomAdaSelSketchConf(
          cmapSize = 20, cmapNo = 2, cmapStart = Some(-6.0), cmapEnd = Some(-3.0),
          counterSize = 20, counterNo = 2,
          bufferSize = bufferSize,
          startThreshold = Integer.MAX_VALUE
        )
        val underlying = NormalDist.std
        val (_, samples) = underlying.samples(bufferSize)
        val sketch0: AdaSelSketch[Double] = AdaSelSketch.empty[Double]
        val sketch1 = sketch0.updateInOrder(samples)
        val diagnose = AdaSelSketch.diagnose(sketch1.asInstanceOf[AdaSelSketch[Double]])

        if(!diagnose) ko(s"diagnose: $diagnose") else ok
      }

      "broad" in {
        val bufferSize = 50
        implicit val conf: CustomAdaSelSketchConf = CustomAdaSelSketchConf(
          cmapSize = 20, cmapNo = 2, cmapStart = Some(-100.0), cmapEnd = Some(100.0),
          counterSize = 20, counterNo = 2,
          bufferSize = bufferSize,
          startThreshold = Integer.MAX_VALUE
        )
        val underlying = NormalDist.std
        val (_, samples) = underlying.samples(bufferSize)
        val sketch0: AdaSelSketch[Double] = AdaSelSketch.empty[Double]
        val sketch1 = sketch0.updateInOrder(samples)
        val diagnose = AdaSelSketch.diagnose(sketch1.asInstanceOf[AdaSelSketch[Double]])

        if(!diagnose) ko(s"diagnose: $diagnose") else ok
      }

    }

  }

}
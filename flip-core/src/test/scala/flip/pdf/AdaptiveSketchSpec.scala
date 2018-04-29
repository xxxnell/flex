package flip.pdf

import flip.implicits._
import flip.conf.CustomAdaPerSketchConf
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.pdf.Buffer.syntax._

class AdaptiveSketchSpec extends Specification with ScalaCheck {

  "AdaptiveSketch" should {

    "append" in {
      val bufferSize = 50
      implicit val conf: CustomAdaPerSketchConf = CustomAdaPerSketchConf(
        bufferSize = bufferSize
      )
      val sketch0 = Sketch.empty[Double]
      val as = List.fill(bufferSize)(2.0)
      val sketch1 = sketch0.narrowUpdate(as: _*)
      val (sketch2, rem) = AdaptiveSketch.append(sketch1.asInstanceOf[AdaptiveSketch[Double]], (10.0, 10.0) :: Nil)

      if(rem.size != 1) ko(rem.toString)
      else if(sketch2.buffer.size != bufferSize) ko(sketch2.buffer.size.toString)
      else ok
    }

  }

}
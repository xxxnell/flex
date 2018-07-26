package flip.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck

class SimpleSketchSpec extends Specification with ScalaCheck {

  "SimpleSketch" should {

    "construct" in {
      val sketch = SimpleSketch.empty[Double]
      val sampling = sketch.pdfSampling
      val cond1 = sampling.records.forall { case (range, value) => !value.isNaN }

      if(!cond1) ko(s"Empty SimpleSketch: $sampling")
      else ok
    }

  }

}
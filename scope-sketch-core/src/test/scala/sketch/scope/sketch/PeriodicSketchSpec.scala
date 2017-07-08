package sketch.scope.sketch

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class PeriodicSketchSpec extends Specification with ScalaCheck {

  "PeroidicSketch" should {

    "rearranged" in {
      (for {
        sketch <- PeriodicSketchGen.periodicSketchSample
        utdSketch <- PeriodicSketch.primitiveUpdate(sketch, 1.0)
        rearranged <- PeriodicSketch.rearrange(utdSketch)
      } yield rearranged)
        .fold(ko)(sketch => ok)
    }

  }

}

object PeriodicSketchGen {

  def periodicSketchGen: Gen[PeriodicSketch] = for {
    caDepth <- Gen.choose(1, 10)
    caSize <- Gen.choose(100, 10000)
    coDepth <- Gen.choose(1, 10)
    coSize <- Gen.choose(100, 10000)
  } yield PeriodicSketch.empty(caDepth, caSize, coDepth, coSize)

  def periodicSketchSample: Option[PeriodicSketch] = periodicSketchGen.sample

}
package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class ContSketchSpec extends Specification with ScalaCheck {

  "ContSketch" should {

    todo

  }

}

object ContSketchGen {

  def intContSketchGen: Gen[ContSketch[Int]] = for {
    measure <- MeasureGen.intMeasureGen
    caDepth <- Gen.choose(1, 10)
    caSize <- Gen.choose(100, 10000)
    coDepth <- Gen.choose(1, 10)
    coSize <- Gen.choose(100, 10000)
  } yield ContSketch.empty(measure, caDepth, caSize, coDepth, coSize)

  def contSketchSample: Option[ContSketch[Int]] = intContSketchGen.sample

}
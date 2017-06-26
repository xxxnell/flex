package sketch.scope.sketch

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable._

/**
  * Licensed by Probe Technology, Inc.
  */
class SketchSpec extends Specification with ScalaCheck {

  "Sketch" should {

    "update" in {
      (for {
        sketch <- SketchGen.sketchSample
        updated <- sketch.update(1)
      } yield updated)
        .fold(ko)(sketch => ok)
    }

    "get" in {
      (for {
        sketch <- SketchGen.sketchSample
        updated <- sketch.update(1)
        res <- sketch.count(0, 10)
      } yield res)
        .fold(ko)(sketch => ok)
    }

  }

}

object SketchGen {

  def sketchGen: Gen[Sketch] = for {
    caDepth <- Gen.choose(1, 10)
    caSize <- Gen.choose(100, 10000)
    coDepth <- Gen.choose(1, 10)
    coSize <- Gen.choose(100, 10000)
  } yield Sketch.empty(caDepth, caSize, coDepth, coSize)

  def sketchSample: Option[Sketch] = sketchGen.sample

}
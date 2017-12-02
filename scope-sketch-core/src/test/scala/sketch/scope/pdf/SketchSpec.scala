package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable._

/**
  * Licensed by Probe Technology, Inc.
  */
class SketchSpec extends Specification with ScalaCheck {

  "Sketch" should {

    "Prop ops" in {

      "update int" in {
        (for {
          sketch <- SketchGen.intSketchSample
          updated <- sketch.update(1)
        } yield updated)
          .fold(ko)(sketch => ok)
      }

      "update boolean" in {
        (for {
          sketch <- SketchGen.booleanSketchSample
          updated <- sketch.update(true)
        } yield updated)
          .fold(ko)(sketch => ok)
      }

      "get" in {
        (for {
          sketch <- SketchGen.intSketchSample
          updated <- sketch.update(1)
          res <- sketch.count(0, 10)
        } yield res)
          .fold(ko)(sketch => ok)
      }

    }

    "Monad Ops" in {

      "map" in {
        (for {
          sketch <- SketchGen.intSketchSample
          utdSketch = sketch.map(i => -1 * i)
        } yield utdSketch)
          .fold(ko)(sketch => ok)
      }

      "flatMap" in {
        (for {
          sketch <- SketchGen.intSketchSample
          utdSketch = sketch.flatMap(i => NormalDist[Double](identity, i, 1))
        } yield utdSketch)
          .fold(ko)(sketch => ok)
      }

    }

  }

}

object SketchGen {

  def intSketchGen: Gen[Sketch[Int]] = sketchGen(MeasureGen.intMeasure)

  def booleanSketchGen: Gen[Sketch[Boolean]] = sketchGen(MeasureGen.booleanMeasure)

  def sketchGen[A](measure: A => Double): Gen[Sketch[A]] = for {
    caDepth <- Gen.choose(1, 10)
    caSize <- Gen.choose(100, 10000)
    coDepth <- Gen.choose(1, 10)
    coSize <- Gen.choose(100, 10000)
  } yield Sketch.empty(measure, caDepth, caSize, coDepth, coSize)

  def intSketchSample: Option[Sketch[Int]] = intSketchGen.sample

  def booleanSketchSample: Option[Sketch[Boolean]] = booleanSketchGen.sample

}
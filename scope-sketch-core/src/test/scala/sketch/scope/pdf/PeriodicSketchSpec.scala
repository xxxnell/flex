package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.conf.SketchConfGen

/**
  * Licensed by Probe Technology, Inc.
  */
class PeriodicSketchSpec extends Specification with ScalaCheck {

  "PeroidicSketch" should {

    "rearranged" in {

      todo

//      (for {
//        sketch <- PeriodicSketchGen.periodicSketchSample
//        utdSketch <- PeriodicSketch.primUpdate(sketch, 1.0)
//        rearranged <- PeriodicSketch.rearrange(utdSketch)
//      } yield rearranged)
//        .fold(ko)(sketch => ok)
    }

  }

}

object PeriodicSketchGen {

  def intPeriodicSketchGen: Gen[PeriodicSketch[Int]] = for {
    measure <- MeasureGen.intMeasureGen
    conf <- SketchConfGen.sketchConfGen
  } yield PeriodicSketch.empty(measure, conf)

  def periodicSketchSample: Option[PeriodicSketch[Int]] = intPeriodicSketchGen.sample

}
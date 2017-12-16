package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.conf.SketchConfGen
import sketch.scope.measure.MeasureGen

/**
  * Licensed by Probe Technology, Inc.
  */
class RecurSketchSpec extends Specification with ScalaCheck {

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

  def intPeriodicSketchGen: Gen[RecurSketch[Int]] = for {
    measure <- MeasureGen.intMeasureGen
    conf <- SketchConfGen.sketchConfGen
  } yield RecurSketch.empty(measure, conf)

  def periodicSketchSample: Option[RecurSketch[Int]] = intPeriodicSketchGen.sample

}
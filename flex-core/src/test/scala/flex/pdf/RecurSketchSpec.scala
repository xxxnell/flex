package flex.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flex.conf.SketchConfGen
import flex.measure.MeasureGen

class RecurSketchSpec extends Specification with ScalaCheck {

  "PeroidicSketch" should {

    "rebuild" in {

      todo

//      (for {
//        sketch <- PeriodicSketchGen.periodicSketchSample
//        utdSketch <- PeriodicSketch.primUpdate(sketch, 1.0)
//        rebuilded <- PeriodicSketch.rebuild(utdSketch)
//      } yield rebuilded)
//        .fold(ko)(sketch => ok)
    }

  }

}

object RecurSketchGen {

//  def intRecurSketchGen: Gen[RecurSketch[Int]] = for {
//    measure <- MeasureGen.intMeasureGen
//    conf <- SketchConfGen.sketchConfGen
//  } yield RecurSketch.empty(measure, conf)
//
//  def recurSketchSample: Option[RecurSketch[Int]] = intRecurSketchGen.sample

}
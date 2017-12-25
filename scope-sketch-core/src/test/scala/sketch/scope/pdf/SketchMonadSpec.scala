package sketch.scope.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.conf.{CmapConf, CounterConf, SketchConf}
import sketch.scope.measure.doubleMeasure

/**
  * Licensed by Probe Technology, Inc.
  */
class SketchMonadSpec extends Specification with ScalaCheck {

  "Sketch" should {

    "Monad Ops" in {

      "map" in {

        "basic" in {
          val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 1, 0, 10)
          val (counterSize, counterNo) = (2, 1)
          implicit val conf: SketchConf = SketchConf(
            CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
            CounterConf(counterSize, counterNo)
          )
          val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

          sketch0.map(i => -1 * i)
          ok
        }

//        "arbitrary" in {
//          (for {
//            sketch <- SketchGen.intSketchSample
//            utdSketch = sketch.map(i => -1 * i)
//          } yield utdSketch)
//            .fold(ko)(sketch => ok)
//        }

      }

      "flatMap" in {

        "basic" in {
          val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 1, 0, 10)
          val (counterSize, counterNo) = (2, 1)
          implicit val conf: SketchConf = SketchConf(
            CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
            CounterConf(counterSize, counterNo)
          )
          val sketch0 = Sketch.empty[Double](doubleMeasure, conf)

          sketch0.flatMap(i => NormalDist[Double](i, 1))
          ok
        }

//        "arbitrary" in {
//          (for {
//            sketch <- SketchGen.intSketchSample
//            utdSketch = sketch.flatMap(i => NormalDist[Double](doubleMeasure, i, 1))
//          } yield utdSketch)
//            .fold(ko)(sketch => ok)
//        }

      }

    }

  }

}
package flip.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.conf._
import flip._

class SketchMonadSpec extends Specification with ScalaCheck {

  "Sketch" should {

    "Monad Ops" in {

      "map" in {

        "basic" in {
          val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 1, Some(0d), Some(10d))
          val (counterSize, counterNo) = (2, 1)
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
            counterSize = counterSize, counterNo = counterNo
          )
          val sketch0 = Sketch.empty[Double]

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
          val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 1, Some(0d), Some(10d))
          val (counterSize, counterNo) = (2, 1)
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
            counterSize = counterSize, counterNo = counterNo
          )
          val sketch0 = Sketch.empty[Double]

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
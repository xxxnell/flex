package flip.conf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

class SketchConfSpec extends Specification with ScalaCheck {

  "ShetchConf" should {

    "default" in {
      implicitly[SketchConf] must beAnInstanceOf[SketchConf]
    }

    "customize locally" in {
      val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(0d), Some(10d))
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: CustomSketchConf = CustomSketchConf(
        cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
        counterSize = counterSize, counterNo = counterNo
      )

      implicitly[SketchConf] must equalTo(conf)
    }

  }

}

object SketchConfGen {

  def sketchConfGen: Gen[PeriodicSketchConf] = for {
    cmapNo <- Gen.choose(1, 10)
    cmapSize <- Gen.choose(100, 10000)
    counterNo <- Gen.choose(1, 10)
    counterSize <- Gen.choose(100, 10000)
  } yield CustomSketchConf(cmapNo = cmapNo, cmapSize= cmapSize, counterNo = counterNo, counterSize = counterSize)

//  def periodicSketchConfGen: Gen[PeriodicSketchConf] = for {
//
//  } yield PeriodicSketchConf

//  def cmapConfGen: Gen[CmapConf] = uniformCmapConfGen
//
//  def uniformCmapConfGen: Gen[UniformCmapConf] = for {
//    cmapNo <- Gen.choose(1, 10)
//    cmapSize <- Gen.choose(100, 10000)
//  } yield UniformCmapConf(cmapSize, cmapNo, None, None)
//
//  def counterConfGen: Gen[CounterConf] = for {
//    counterNo <- Gen.choose(1, 10)
//    counterSize <- Gen.choose(100, 10000)
//  } yield CounterConf(counterSize, counterNo)

}
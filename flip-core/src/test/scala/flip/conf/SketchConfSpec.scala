package flip.conf

import flip.conf.pdf.{CustomAdaPerSketchConf, CustomPeriodicSketchConf, PeriodicSketchConf}
import flip.implicits._
import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

class SketchConfSpec extends Specification with ScalaCheck {

  "ShetchConf" should {

    "default" in {
      implicitly[flip.conf.pdf.SketchConf] must beAnInstanceOf[flip.conf.pdf.SketchConf]
    }

    "customize locally" in {
      val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(0d), Some(10d))
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: SketchConf = SketchConf(
        cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
        counterSize = counterSize, counterNo = counterNo
      )

      implicitly[flip.conf.pdf.SketchConf] must equalTo(conf)
    }

  }

}

object SketchConfGen {

  def sketchConfGen: Gen[SketchConf] = for {
    cmapNo <- Gen.choose(1, 10)
    cmapSize <- Gen.choose(100, 10000)
    counterNo <- Gen.choose(1, 10)
    counterSize <- Gen.choose(100, 10000)
  } yield SketchConf(cmapNo = cmapNo, cmapSize= cmapSize, counterNo = counterNo, counterSize = counterSize)

  def periodicSketchConfGen: Gen[PeriodicSketchConf] = for {
    cmapNo <- Gen.choose(1, 10)
    cmapSize <- Gen.choose(100, 10000)
    counterNo <- Gen.choose(1, 10)
    counterSize <- Gen.choose(100, 10000)
  } yield CustomAdaPerSketchConf(cmapNo = cmapNo, cmapSize= cmapSize, counterNo = counterNo, counterSize = counterSize)

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
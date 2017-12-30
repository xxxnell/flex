package sketch.scope.conf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class SketchConfSpec extends Specification with ScalaCheck {

  "ShetchConf" should {

    "default" in {
      implicitly[SketchConf] must beAnInstanceOf[SketchConf]
    }

    "customize locally" in {
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 2, 0, 10)
      val (counterSize, counterNo) = (8, 2)
      implicit val conf: CustomSketchConf = SketchConf(
        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
        CounterConf(counterSize, counterNo)
      )

      implicitly[SketchConf] must equalTo(conf)
    }

  }

}

object SketchConfGen {

  def sketchConfGen: Gen[SketchConf] = for {
    cmapConf <- cmapConfGen
    counterConf <- counterConfGen
  } yield SketchConf(cmapConf, counterConf)

  def cmapConfGen: Gen[CmapConf] = uniformCmapConfGen

  def uniformCmapConfGen: Gen[UniformCmapConf] = for {
    cmapNo <- Gen.choose(1, 10)
    cmapSize <- Gen.choose(100, 10000)
  } yield UniformCmapConf(cmapSize, cmapNo, None, None)

  def counterConfGen: Gen[CounterConf] = for {
    counterNo <- Gen.choose(1, 10)
    counterSize <- Gen.choose(100, 10000)
  } yield CounterConf(counterSize, counterNo)

}
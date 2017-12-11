package sketch.scope.conf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class SketchConfSpec extends Specification with ScalaCheck {

  "ShetchConf" should todo

}

object SketchConfGen {

  def sketchConfGen: Gen[SketchConf] = for {
    cmapNo <- Gen.choose(1, 10)
    cmapSize <- Gen.choose(100, 10000)
    counterNo <- Gen.choose(1, 10)
    counterSize <- Gen.choose(100, 10000)
  } yield SketchConf(cmapSize, cmapNo, Double.MinValue, Double.MaxValue, counterSize, counterNo)

}
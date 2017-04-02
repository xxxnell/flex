package sketch.scope.sketch

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable._

/**
  * Licensed by Probe Technology, Inc.
  */
class SketchSpec extends Specification with ScalaCheck {

  "Sketch" should todo

}

object SketchGen {

  def sketchGen: Gen[Sketch[_]] = ???

}
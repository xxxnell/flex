package sketch.scope.counter

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck

/**
  * Licensed by Probe Technology, Inc.
  */
class ListCounterSpec extends Specification with ScalaCheck {

  "ListCounter" in todo

}

object ListCounterGen {

  def listCounterGen: Gen[ListCounter] = ???

}
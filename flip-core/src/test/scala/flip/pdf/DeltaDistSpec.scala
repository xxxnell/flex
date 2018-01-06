package flip.pdf

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.measure._
import flip._

class DeltaDistSpec extends Specification with ScalaCheck {

  "DeltaDist" should {

    "sample" in {
      implicit val deltaDistA: Arbitrary[DeltaDist[Double]] = DeltaDistGen.doubleDeltaDistA

      prop { (dist0: DeltaDist[Double]) =>
        val (dist1, sample) = dist0.sample
        if(dist0.pole == sample) ok else ko
      }.setArbitrary(deltaDistA)
    }

  }

}

object DeltaDistGen {

  def deltaDistGen[A](measure: Measure[A]): Gen[DeltaDist[A]] = for {
    mean <- Arbitrary.arbitrary[Double]
  } yield DeltaDist(measure, mean)

  def doubleDeltaDistGen: Gen[DeltaDist[Double]] = deltaDistGen(doubleMeasure)

  def doubleDeltaDistA: Arbitrary[DeltaDist[Double]] = Arbitrary(doubleDeltaDistGen)

}
package sketch.scope.pdf

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.measure._
import sketch.scope._

/**
  * Licensed by Probe Technology, Inc.
  */
class NormalDistSpec extends Specification with ScalaCheck {

  "NormalDist" should {

    "ops" in {

      "sample" in {

        "variation" in {
          implicit val normalDistA: Arbitrary[NormalDist[Double]] = NormalDistGen.doubleNormalDistA

          prop { (dist0: NormalDist[Double]) =>
            val (dist1, sample1) = dist0.sample
            val (dist2, sample2) = dist1.sample

            if(sample1 != sample2) ok else ko(s"sample1: $sample1, sample2: $sample2")
          }.setArbitrary(normalDistA)
        }

        "consistency" in {
          implicit val normalDistA: Arbitrary[NormalDist[Double]] = NormalDistGen.doubleNormalDistA

          prop { (dist: NormalDist[Double]) =>
            val (_, sample1) = dist.sample
            val (_, sample2) = dist.sample

            if(sample1 == sample2) ok else ko
          }.setArbitrary(normalDistA)
        }

      }

      "samples" in {

        "basic" in {

//          implicit val normalDistA: Arbitrary[NormalDist[Double]] = NormalDistGen.doubleNormalDistA
//
//          prop { (dist: NormalDist[Double]) =>
//            val (_, samples) = dist.samples(100)
//            samples.foreach(sample => println(sample))
//
//            ok
//          }.setArbitrary(normalDistA)

          NormalDistGen.doubleNormalDistGen.sample.fold(ko)(dist => {
            val (_, samples) = dist.samples(100)
//            samples.foreach(sample => println(sample))
            ok
          })
        }

      }

    }

  }

}

object NormalDistGen {

  def normalDistGen[A](measure: Measure[A]): Gen[NormalDist[A]] = for {
    mean <- Gen.choose(-1000d, 1000d)
    variance <- Gen.choose(0.1d, 100d)
  } yield NormalDist(measure, mean, variance)

  def doubleNormalDistGen: Gen[NormalDist[Double]] = normalDistGen(doubleMeasure)

  def doubleNormalDistA: Arbitrary[NormalDist[Double]] = Arbitrary(doubleNormalDistGen)

}
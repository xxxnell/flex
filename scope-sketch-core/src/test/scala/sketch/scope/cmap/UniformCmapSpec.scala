package sketch.scope.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.pdf.Prim

/**
  * Licensed by Probe Technology, Inc.
  */
class UniformCmapSpec extends Specification with ScalaCheck {

//  implicit val cmapGen: Arbitrary[(Int, UniformCmap)] = Arbitrary(UniformCmapGen.uniformCmapGen)
//
//  prop { (sizeCmap: (Int, UniformCmap) ) =>
//    todo
//  }.setArbitrary(cmapGen)

  "UniformCmap" should {

    "apply" in {

      "UniformCmap: unbounded" in {
        implicit val cmapGen: Arbitrary[(Int, UniformCmap)] = Arbitrary(UniformCmapGen.uniformCmapGen1)

        prop { (cmapGenProps: (Int, UniformCmap) ) =>
          val (size, cmap) = cmapGenProps

          val cond1 = cmap(Double.MinValue) == 0
          val cond2 = cmap(Double.MaxValue) == size - 1
          if (cond1 && cond2) ok
          else ko(
            s"cmap(Double.MinValue): ${cmap(Double.MinValue)}, " +
              s"cmap(Double.MaxValue): ${cmap(Double.MaxValue)}"
          )
        }.setArbitrary(cmapGen)
      }

      "UniformCmap: lower bound" in {
        implicit val cmapGen: Arbitrary[(Int, Prim, UniformCmap)] = Arbitrary(UniformCmapGen.uniformCmapGen2)

        prop { (cmapGenProps: (Int, Prim, UniformCmap) ) =>
          val (size, start, cmap) = cmapGenProps

          val cond1 = cmap(Double.MinValue) == 0
          val cond2 = cmap(Double.MaxValue) == size - 1
          val cond3 = cmap(start) == 1
          if(cond1 && cond2 && cond3) ok
          else ko(
            s"cmap(Double.MinValue): ${cmap(Double.MinValue)}, " +
              s"cmap(Double.MaxValue): ${cmap(Double.MaxValue)}, " +
              s"cmap(start): ${cmap(start)}"
          )
        }.setArbitrary(cmapGen)
      }

      "UniformCmap: upper bound" in {
        implicit val cmapGen: Arbitrary[(Int, Prim, UniformCmap)] = Arbitrary(UniformCmapGen.uniformCmapGen3)

        prop { (cmapGenProps: (Int, Prim, UniformCmap) ) =>
          val (size, end, cmap) = cmapGenProps

          val cond1 = cmap(Double.MinValue) == 0
          val cond2 = cmap(Double.MaxValue) == size - 1
          val cond3 = cmap(end) == size - 1
          if(cond1 && cond2 && cond3) ok
          else ko(
            s"cmap(Double.MinValue): ${cmap(Double.MinValue)}, " +
              s"cmap(Double.MaxValue): ${cmap(Double.MaxValue)}, " +
              s"cmap(end): ${cmap(end)}"
          )
        }.setArbitrary(cmapGen)
      }

      "UniformCmap: lower and upper bound" in {
        implicit val cmapGen: Arbitrary[(Int, Prim, Prim, UniformCmap)] = Arbitrary(UniformCmapGen.uniformCmapGen4)

        prop { (cmapGenProps: (Int, Prim, Prim, UniformCmap) ) =>
          val (size, start, end, cmap) = cmapGenProps

          val cond1 = cmap(Double.MinValue) == 0
          val cond2 = cmap(Double.MaxValue) == size - 1
          val cond3 = cmap(start) == 1
          val cond4 = cmap(end) == size - 1
          if(cond1 && cond2 && cond3 && cond4) ok
          else ko(
            s"cmap(Double.MinValue): ${cmap(Double.MinValue)}, " +
              s"cmap(Double.MaxValue): ${cmap(Double.MaxValue)}, " +
              s"cmap(start): ${cmap(start)}, " +
              s"cmap(end): ${cmap(end)}"
          )
        }.setArbitrary(cmapGen)
      }

    }

  }

}

object UniformCmapGen {

  def uniformCmapGen: Gen[(Int, UniformCmap)] = Gen.oneOf(
    uniformCmapGen1,
    uniformCmapGen2.map { case (size, _, cmap) => (size, cmap) },
    uniformCmapGen3.map { case (size, _, cmap) => (size, cmap) },
    uniformCmapGen4.map { case (size, _, _, cmap) => (size, cmap) }
  )

  /**
    * @return Gen of (size, cmap)
    * */
  def uniformCmapGen1: Gen[(Int, UniformCmap)] = for {
    n <- Gen.choose(0, Math.pow(2, 19).toInt)
  } yield (n, UniformCmap(n))

  /**
    * @return Gen of (size, start, cmap)
    * */
  def uniformCmapGen2: Gen[(Int, Prim, UniformCmap)] = for {
    n <- Gen.choose(0, Math.pow(2, 19).toInt)
    start <- Arbitrary.arbitrary[Prim]
  } yield (n, start, UniformCmap(n, Some(start), None))

  /**
    * @return Gen of (size, end, cmap)
    * */
  def uniformCmapGen3: Gen[(Int, Prim, UniformCmap)] = for {
    n <- Gen.choose(0, Math.pow(2, 19).toInt)
    end <- Arbitrary.arbitrary[Prim]
  } yield (n, end, UniformCmap(n, None, Some(end)))

  /**
    * @return Gen of (size, start, end, cmap)
    * */
  def uniformCmapGen4: Gen[(Int, Prim, Prim, UniformCmap)] = for {
    n <- Gen.choose(0, Math.pow(2, 19).toInt)
    start <- Arbitrary.arbitrary[Prim]
    end <- Arbitrary.arbitrary[Prim]
    if start < end
  } yield (n, start, end, UniformCmap(n, Some(start), Some(end)))

  def uniformCmapA: Arbitrary[(Int, UniformCmap)] = Arbitrary(uniformCmapGen1)

}
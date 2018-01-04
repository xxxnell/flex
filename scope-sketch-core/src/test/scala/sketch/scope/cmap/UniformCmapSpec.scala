package sketch.scope.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.pdf.Prim

class UniformCmapSpec extends Specification with ScalaCheck {

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
            s"size: $size, " +
              s"cmap(Double.MinValue): ${cmap(Double.MinValue)}, " +
              s"cmap(Double.MaxValue): ${cmap(Double.MaxValue)}"
          )
        }.setArbitrary(cmapGen)
      }

      "UniformCmap: lower bound" in {

        "boundaries" in {
          implicit val cmapGen: Arbitrary[(Int, Prim, UniformCmap)] = Arbitrary(UniformCmapGen.uniformCmapGen2)

          prop { (cmapGenProps: (Int, Prim, UniformCmap) ) =>
            val (size, start, cmap) = cmapGenProps
            val delta = ((BigDecimal(Double.MaxValue) - BigDecimal(start)) / size * 0.001).toDouble

            val cond1 = cmap(Double.MinValue) == 0
            val cond2 = cmap(Double.MaxValue) == size - 1
            val cond3 = cmap(start + delta) == 1
            if(cond1 && cond2 && cond3) ok
            else ko(
              s"size: $size, " +
                s"start: $start, " +
                s"start + delta: ${start + delta}, " +
                s"cmap(Double.MinValue): ${cmap(Double.MinValue)}, " +
                s"cmap(Double.MaxValue): ${cmap(Double.MaxValue)}, " +
                s"cmap(start + delta): ${cmap(start + delta)}"
            )
          }.setArbitrary(cmapGen)
        }

      }

      "UniformCmap: upper bound" in {

        "boundaries" in {
          implicit val cmapGen: Arbitrary[(Int, Prim, UniformCmap)] = Arbitrary(UniformCmapGen.uniformCmapGen3)

          prop { (cmapGenProps: (Int, Prim, UniformCmap) ) =>
            val (size, end, cmap) = cmapGenProps
            val delta = ((BigDecimal(end) - BigDecimal(Double.MinValue)) / size * 0.001).toDouble

            val cond1 = cmap(Double.MinValue) == 0
            val cond2 = cmap(Double.MaxValue) == size - 1
            val cond3 = cmap(end + delta) == size - 1
            if(cond1 && cond2 && cond3) ok
            else ko(
              s"size: $size, " +
                s"end: $end, " +
                s"cmap(Double.MinValue): ${cmap(Double.MinValue)}, " +
                s"cmap(Double.MaxValue): ${cmap(Double.MaxValue)}, " +
                s"cmap(end + delta): ${cmap(end + delta)}"
            )
          }.setArbitrary(cmapGen)
        }

        "sample" in {
          val (n, end) = (3, 0)
          val cmap = UniformCmap(n, None, Some(end))
          val cond1 = cmap(Double.MinValue / 2) == 1
          val cond2 = cmap(0) == 2

          if(cond1 && cond2) ok else ko(s"n: $n, end: $end, divider: ${cmap.divider}")
        }

      }

      "UniformCmap: lower and upper bound" in {
        implicit val cmapGen: Arbitrary[(Int, Prim, Prim, UniformCmap)] = Arbitrary(UniformCmapGen.uniformCmapGen4)

        prop { (cmapGenProps: (Int, Prim, Prim, UniformCmap) ) =>
          val (size, start, end, cmap) = cmapGenProps
          val delta = ((BigDecimal(end) - BigDecimal(start)) / size * 0.001).toDouble

          val cond1 = cmap(Double.MinValue) == 0
          val cond2 = cmap(Double.MaxValue) == size - 1
          val cond3 = cmap(start + delta) == 1
          val cond4 = cmap(end + delta) == size - 1
          if(cond1 && cond2 && cond3 && cond4) ok
          else ko(
            s"size: $size, " +
              s"start: $start, " +
              s"end: $end, " +
              s"cmap(Double.MinValue): ${cmap(Double.MinValue)}, " +
              s"cmap(Double.MaxValue): ${cmap(Double.MaxValue)}, " +
              s"cmap(start + delta): ${cmap(start + delta)}, " +
              s"cmap(end + delta): ${cmap(end + delta)}"
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
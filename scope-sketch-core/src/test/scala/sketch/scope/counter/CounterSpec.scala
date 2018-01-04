package sketch.scope.counter

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable._

class CounterSpec extends Specification with ScalaCheck {

  "Counter" in {

    "ops" in {

      "update" in {
        implicit val counterCdimCountA: Arbitrary[(Counter, CDim)] = CounterGen.counterCdimA
        implicit val countGenA: Arbitrary[Double] = CounterGen.countA

        prop { (counterCdimCount: (Counter, CDim), count : Double) =>
          val (counter, cdim) = counterCdimCount
          val countergen = counter.update(cdim, count)
          val updatedcount = countergen.get.get(cdim).get
          val koMsg =
            s"count : $count " +
            s"counterupdated : $updatedcount"

          if( updatedcount == count) ok
          else ko(koMsg)
        }.setArbitrary1(counterCdimCountA)
          .setArbitrary2(countGenA)
      }

    }

    "laws" in {

      "get" in {

        "empty" in {
          implicit val counterCdimA: Arbitrary[(Counter, CDim)] = CounterGen.counterCdimA

          prop { (counterCdim: (Counter, CDim)) =>
            val (counter, cdim) = counterCdim
            val corr = 1e-5f
            val koMsg =
              s"Cannot get the recorded count: " +
                s"counter size -> ${counter.size}, cdim -> $cdim"

            (for {
              recorded <- counter.get(cdim)
            } yield recorded)
              .fold(ko(koMsg))(recorded => recorded must beCloseTo(0d, corr))
          }.setArbitrary(counterCdimA)
        }

        "filled" in {
          implicit val counterCdimA: Arbitrary[(Counter, CDim)] = CounterGen.counterCdimA

          prop { (counterCdim: (Counter, CDim), count: Double) =>
            val (counter, cdim) = counterCdim
            val corr = 1e-5f
            val koMsg =
              s"Cannot get the recorded count: " +
                s"counter size -> ${counter.size}, cdim -> $cdim, count -> $count"

            (for {
              counter <- counter.update(cdim, count)
              recorded <- counter.get(cdim)
            } yield recorded)
              .fold(ko(koMsg))(recorded => recorded must beCloseTo(count, corr))
          }.setArbitrary1(counterCdimA)
        }

      }

      "size" in {
        implicit val counterCdimA: Arbitrary[(Counter, CDim)] = CounterGen.counterSizeA

        prop { (counterSize: (Counter, Int)) =>
          val (counter, size) = counterSize
          //val corr = 1e-5f
          val koMsg =
            s"Cannot get the recorded count: " +
              s"counter size -> ${counter.size}, cdim -> $size"
          if( counter.size == size) ok
          else ko(koMsg)
        }.setArbitrary(counterCdimA)
      }

    }

  }

}

object CounterGen {

  val maxSize: Int = 10

  def sizeGen: Gen[Int] = Gen.choose(0, maxSize)

  def sizeA: Arbitrary[Int] = Arbitrary(sizeGen)

  def counterGen(size: Int): Gen[Counter] = {
    if (size > 0) {
      Gen.const(Counter.empty(size))
    } else Gen.const(Counter.empty(0))
  }

  def counterA(size: Int): Arbitrary[Counter] = Arbitrary(counterGen(size))

  def cdimGen(size: Int): Gen[CDim] = {
    if (size > 0) {
      Gen.choose(0, size - 1)
    } else Gen.const(0)
  }

  def cdimA(size: Int): Arbitrary[CDim] = Arbitrary(cdimGen(size))

  def counterCdimGen: Gen[(Counter, CDim)] = for {
    size <- CounterGen.sizeGen
    if size > 0
    counter <- CounterGen.counterGen(size)
    cdim <- CounterGen.cdimGen(size)
  } yield (counter, cdim)

  def counterSizeGen: Gen[(Counter, Int)] = for {
    size <- CounterGen.sizeGen
    if size > 0
    counter <- CounterGen.counterGen(size)
  } yield (counter, size)

  def countGen: Gen[Double] = for {
    count <- Gen.choose(Double.MinValue, Double.MaxValue)
  } yield count

  def counterCdimA: Arbitrary[(Counter, CDim)] = Arbitrary(counterCdimGen)

  def counterSizeA: Arbitrary[(Counter, Int)] = Arbitrary(counterSizeGen)

  def countA: Arbitrary[Double] = Arbitrary(countGen)
}
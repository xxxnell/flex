package flex.counter

import flex.measure.syntax._
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
          val utdCounter = counter.update(cdim, count)
          val gotCount = utdCounter.get(cdim)

          if(gotCount != count) ko
          else ok
        }.setArbitrary1(counterCdimCountA).setArbitrary2(countGenA)
      }

    }

    "laws" in {

      "get" in {

        "empty" in {
          implicit val counterCdimA: Arbitrary[(Counter, CDim)] = CounterGen.counterCdimA

          prop { (counterCdim: (Counter, CDim)) =>
            val (counter, cdim) = counterCdim
            val recorded = counter.get(cdim)
            val cond = recorded ~= 0.0

            if(!cond) ko else ok
          }.setArbitrary(counterCdimA)
        }

        "filled" in {
          implicit val counterCdimA: Arbitrary[(Counter, CDim)] = CounterGen.counterCdimA

          prop { (counterCdim: (Counter, CDim), count: Double) =>
            val (counter, cdim) = counterCdim
            val utdCounter = counter.update(cdim, count)
            val recorded = utdCounter.get(cdim)
            val cond = recorded ~= count

            if(!cond) ko else ok
          }.setArbitrary1(counterCdimA)
        }

      }

      "size" in {
        implicit val counterCdimA: Arbitrary[(Counter, CDim)] = CounterGen.counterSizeA

        prop { (counterSize: (Counter, Int)) =>
          val (counter, size) = counterSize
          if(counter.size == size) ok else ko
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
package flip.pdf.arithmetic

import flip.implicits._
import flip.measure.syntax._
import org.specs2.mutable._
import org.specs2.ScalaCheck

class SumSpec extends Specification with ScalaCheck {

  "Sum" should {

    "samples" in {
      val c = 1.0 / 3
      val underlying = { (c, Dist.delta(1)) + (c, Dist.delta(2)) + (c, Dist.delta(3)) }

      val size = 10000
      val (_, samples) = underlying.samples(size)
      val perc1 = samples.count(_ == 1).toDouble / size
      val perc2 = samples.count(_ == 2).toDouble / size
      val perc3 = samples.count(_ == 3).toDouble / size

      val cond1 = perc1 ~= c
      val cond2 = perc2 ~= c
      val cond3 = perc3 ~= c

      if(!cond1 || !cond2 || !cond3) ko(s"results: $perc1, $perc2, $perc3 (expected: $c)")
      else ok
    }

  }

}
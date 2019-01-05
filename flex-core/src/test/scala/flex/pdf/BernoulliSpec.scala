package flex.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck
import flex.pdf.Bernoulli.syntax._

class BernoulliSpec extends Specification with ScalaCheck {

  "Bernoulli" should {

    "sample" in {
      val p = 0.7
      val e = 0.1
      val sampleNo = 100
      val (_, ss) = (1 to sampleNo).foldRight((Bernoulli(p), List.empty[Int])){
        case (_, (b, _ss)) => (b.sample._1, b.sample._2 :: _ss)
      }

      val sperc = ss.count(_ == 1) / ss.length.toDouble
      val cond1 = sperc >= p - e
      val cond2 = sperc <= p + e
      
      if(!cond1) ko(s"Bernoulli(1|$p) == $sperc")
      else if(!cond2) ko(s"Bernoulli(1|$p) == $sperc")
      else ok
    }

  }

}
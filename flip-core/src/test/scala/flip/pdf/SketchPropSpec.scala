package flip.pdf

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable._
import flip.implicits._
import flip.conf._
import flip.measure.Measure
import flip.measure.syntax._

class SketchPropSpec extends Specification with ScalaCheck {

  "Sketch" should {

    "construct" in {

      "structure size" in {
        val cmapSize = 10
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = cmapSize, cmapNo = 2, cmapStart = Some(0d), cmapEnd = Some(10d),
          counterSize = 10, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]

        if(sketch0.cmapSize != cmapSize)
          ko(s"Initialized size of sketch structure is not 1: size = ${sketch0.cmapSize}, expected: $cmapSize")
        else ok
      }

      "SimpleSketch" in {
        implicit val conf: CustomSketchConf = CustomSimpleSketchConf(
          binNo = 10, start = 0d, end = 10d,
          counterSize = 10, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]

        sketch0 must beAnInstanceOf[SimpleSketch[Double]]
      }

    }

    "count" in {

      "basic 1" in {
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(0d), Some(10d))
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.update(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        val count = sketch1.count(1, 5)

        if(count < 10) ok
        else ko(s"count: $count, expected: 5")
      }

      "basic 2" in {
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(0d), Some(10d))
        val (counterSize, counterNo) = (100, 2)
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.update(1, 2, 3, 4, 5, 6, 7, 8, 9)
        val count = sketch1.count(0, 10)

        if(count ~= 9) ok
        else ko(s"count: $count, expected: 9")
      }

      "count smaller space then cmap bound" in {
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(0d), Some(10d))
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.update(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        val count = sketch1.count(1.1, 1.2)

        if(count < 1) ok else ko(s"count: $count, expected: <1")
      }

    }

    "update" in {

      "update int" in {
        (for {
          sketch <- SketchGen.intSketchSample
          updated = sketch.update(1)
        } yield updated)
          .fold(ko)(sketch => ok)
      }

      "update boolean" in {
        (for {
          sketch <- SketchGen.booleanSketchSample
          updated = sketch.update(true)
        } yield updated)
          .fold(ko)(sketch => ok)
      }

    }

    "narrowUpdate" in {

      "basic" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          bufferSize = 100,
          cmapSize = 10, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 8, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.narrowUpdate(List.fill(100)(0.0): _*)
        val sketch11 = sketch1.narrowUpdate(0)
        val count = sketch1.count(-1, 1)

        if(count > 0) ok
        else ko(s"count: $count, expected: 0<x<1")
      }

    }

    "probability" in {

      "empty" in {
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(-10d), Some(10d))
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val sketch0 = Sketch.empty[Double]

        val prob = sketch0.probability(0, 1)

        if(prob > 0) ok
        else ko(s"probability: $prob")
      }

      "from min to 0 after updated" in {
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(0d), Some(10d))
        val (counterSize, counterNo) = (100, 2)
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.update(-1)
        val prob1 = sketch1.probability(Double.MinValue, 0)
        val prob2 = sketch1.probability(0, Double.MaxValue)

        val expectedProb1 = 1d
        val expectedProb2 = 0d

        if(!(prob1 ~= expectedProb1)) ko(s"probability for [-∞, 0]: $prob1, expected: $expectedProb1")
        else if(!(prob2 ~= expectedProb2)) ko(s"probability for [0, +∞]: $prob2, expected: $expectedProb2")
        else ok
      }

      "for normal distribution" in {
        val error = 0.2
        val (start, end) = (0.0, 1.0)
        val dist = NumericDist.normal(0.0, 1.0)
        val expected = dist.probability(start, end)
        val sampleNo = 3000
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (20, 2, Some(-10.0), Some(10.0))

        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd
        )
        val (_, samples) = dist.samples(sampleNo)
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.updateInOrder(samples)
        val prob = sketch1.probability(start, end)

        if(similar(prob, expected, error)) ok
        else ko(s"Estimated probability for [$start, $end]: $prob. expected: $expected")
      }

    }

    "sampling" in {

      "basic" in {
        val cmapStart = Some(-10d)
        val cmapEnd = Some(10d)
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 3, cmapNo = 5, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = 70, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]

        val plot = sketch0.sampling

        val cond1 = plot.records.nonEmpty
        val cond2 = plot.records.forall { case (_, value) => !value.isNaN }
        val cond3 = plot.records.headOption.forall { case (range, _) => range.end ~= cmapStart.value }
        val cond4 = plot.records.lastOption.forall { case (range, _) => range.start ~= cmapEnd.value }

        if(!cond1) ko("Plot record is empty.")
        else if(!cond2) ko("Some value is NaN")
        else if(!cond3) ko(s"${plot.records.headOption} is first range. cmapStart: $cmapStart")
        else if(!cond4) ko(s"${plot.records.lastOption} is last range. cmapEnd: $cmapEnd")
        else ok
      }

    }

    "deepUpdate" in {

      "changing cmap" in {

        "basic" in {
          val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 3, Some(-1d), Some(10d))
          val (counterSize, counterNo) = (8, 2)
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
            counterSize = counterSize, counterNo = counterNo
          )
          val sketch0 = Sketch.empty[Double]
          val sketch1 = sketch0.deepUpdate(1)._1
          val cmap0 = sketch0.youngCmap
          val cmap1 = sketch1.youngCmap

          val cond1 = cmap0.size == cmap1.size
          val cond2 = cmap0 != cmap1

          if(!cond1) ko(s"sketch0 size: ${cmap0.size}, sketch1 size: ${sketch1.youngCmap.size}")
          else if(!cond2) ko(s"cmap1(${cmap0.size}): $cmap0, cmap2(${cmap1.size}): $cmap1")
          else ok
        }

        "2 times" in {
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = 10, cmapNo = 2, cmapStart = Some(-1d), cmapEnd = Some(10d),
            counterSize = 8, counterNo = 2
          )
          val sketch0 = Sketch.empty[Double]
          val sketch1 = sketch0.deepUpdate(1)._1
          val sketch2 = sketch1.deepUpdate(1)._1
          val cmap0 = sketch0.youngCmap
          val cmap1 = sketch1.youngCmap
          val cmap2 = sketch2.youngCmap

          val cond1 = cmap0.size == cmap1.size
          val cond2 = cmap0 != cmap1
          val cond3 = cmap1.size == cmap2.size
          val cond4 = cmap0 != cmap1

          if(!cond1) ko(s"sketch0 size: ${cmap0.size}, sketch1 size: ${cmap1.size}")
          else if(!cond2) ko(s"sketch0 cmap: $cmap0, sketch1 cmap: $cmap1")
          else if(!cond3) ko(s"sketch1 size: ${cmap1.size}, sketch2 size: ${cmap2.size}")
          else if(!cond4) ko(s"sketch1 cmap: $cmap1, sketch2 cmap: $cmap1")
          else ok
        }

      }

      "structure size" in {

        "increasing" in {
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = 10, cmapNo = 2, cmapStart = Some(-1d), cmapEnd = Some(10d),
            counterSize = 8, counterNo = 2
          )
          val sketch0 = Sketch.empty[Double]
          val cmapNo0 = sketch0.cmapNo

          val (sketch1, oldStrO) = sketch0.deepUpdate(1)
          val cmapNo1 = sketch1.cmapNo

          if (cmapNo1 != cmapNo0 + 1)
            ko(s"Updated structure size is not ${cmapNo0 + 1}: size = $cmapNo1")
          else if(oldStrO.isDefined)
            ko("deepUpdate returns old structure.")
          else ok
        }

        "bounded" in {
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = 10, cmapNo = 2, cmapStart = Some(-1d), cmapEnd = Some(10d),
            counterSize = 8, counterNo = 2
          )
          val sketch0 = Sketch.empty[Double]
          val cmapNo0 = sketch0.cmapNo
          val (sketch1, oldStr1) = sketch0.deepUpdate(1)
          val cmapNo1 = sketch1.cmapNo
          val (sketch2, oldStr2) = sketch1.deepUpdate(1)
          val cmapNo2 = sketch2.cmapNo

          if (cmapNo1 != cmapNo0 + 1)
            ko(s"Updated structure size is not ${cmapNo0 + 1}: size = $cmapNo1")
          else if(cmapNo1 != cmapNo2)
            ko(s"Sketch structure size is not bounded: before = $cmapNo1, after = $cmapNo2")
          else ok
        }

      }

    }

    "rearrange" in {

      "basic" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 1000, cmapNo = 5, cmapStart = Some(0d), cmapEnd = Some(10d),
          counterSize = 500, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.narrowUpdate(0.0 to 100.0 by 0.1: _*)
        val sketch2 = sketch1.rearrange

        ok
      }

    }

    "count" in {

      "basic" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 10, cmapNo = 2, cmapStart = Some(0d), cmapEnd = Some(10d),
          counterSize = 8, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.update(1)
        val count = sketch1.count(0, 10)

        if(count > 0) ok else ko(s"count: $count")
      }

      "empty" in {
        val (cmapSize, cmapNo, cmapStart, cmapEnd) = (10, 2, Some(0d), Some(10d))
        val (counterSize, counterNo) = (8, 2)
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = cmapSize, cmapNo = cmapNo, cmapStart = cmapStart, cmapEnd = cmapEnd,
          counterSize = counterSize, counterNo = counterNo
        )
        val sketch0 = Sketch.empty[Double](implicitly[Measure[Double]], conf)

        val count = sketch0.count(0, 10)

        if(count == 0) ok else ko(s"count: $count")
      }

    }

    "sum" in {

      "basic" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 10, cmapNo = 2, cmapStart = Some(0d), cmapEnd = Some(10d),
          counterSize = 100, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.update(1)
        val sum = sketch1.sum

        if(sum > 0 && sum <= 1) ok else ko(s"sum: $sum")
      }

      "after updated" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 10, cmapNo = 2, cmapStart = Some(0d), cmapEnd = Some(10d),
          counterSize = 100, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]

        val sketch1 = sketch0.update(1, 2, 3, 4, 5)
        val sum = sketch1.sum

        if(sum ~= 5) ok else ko(s"sum: $sum, expected: 5")
      }

      "after rearrange with 2 cmap" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 10, cmapNo = 2, cmapStart = Some(0d), cmapEnd = Some(10d),
          counterSize = 100, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]
        val expected = 5 / (1 + math.exp(-1))

        val sketch1 = sketch0.update(1, 2, 3, 4, 5)
        val sketch2 = sketch1.rearrange
        val sum = sketch2.sum

        val cond1 = sum ~= expected

        if(!cond1) ko(s"sum: $sum, expected: $expected")
        else ok
      }

      "after rearrange update" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 10, cmapNo = 2, cmapStart = Some(0d), cmapEnd = Some(10d),
          counterSize = 100, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]
        val expected = 10 / (1 + math.exp(-1))

        val sketch1 = sketch0.update(1, 2, 3, 4, 5)
        val sketch2 = sketch1.rearrange
        val sketch3 = sketch2.update(1, 2, 3, 4, 5)
        val sum = sketch3.sum

        if(sum ~= expected) ok else ko(s"sum: $sum, expected: $expected")
      }

      "after 2 rearrange and update with 3 cmap" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 10, cmapNo = 3, cmapStart = Some(0d), cmapEnd = Some(10d),
          counterSize = 100, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]
        val expected = (10 * math.exp(-1) + 5) / (1 + math.exp(-1))

        val sketch1 = sketch0.update(1, 2, 3, 4, 5)
        val sketch2 = sketch1.rearrange
        val sketch3 = sketch2.rearrange
        val sketch4 = sketch3.update(1, 2, 3, 4, 5)
        val sum = sketch4.sum

        if(sum ~= expected) ok else ko(s"sum: $sum, expected: $expected")
      }

    }

    "fastPdf" in {

      "basic" in {
        implicit val conf: CustomSketchConf = CustomSketchConf(
          cmapSize = 10, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
          counterSize = 10, counterNo = 2
        )
        val sketch0 = Sketch.empty[Double]
        val sketch1 = sketch0.update(0, 1, 1, 2, 3)

        val interpPdf = SamplingDist.interpolationPdf(sketch1, 1d)
        val fastPdf = Sketch.fastPdf(sketch1, 1d)

        if(interpPdf ~= fastPdf) ok else ko(s"interpPdf: $interpPdf, fastPdf: $fastPdf")
      }

      "boundary" in {

        "least" in {
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = 10, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
            counterSize = 10, counterNo = 2
          )
          val sketch0 = Sketch.empty[Double]
          val sketch1 = sketch0.update(0, 1, 1, 2, 3)
          val fastPdf = Sketch.fastPdf(sketch1, Double.MinValue)

          ok
        }

        "largest" in {
          implicit val conf: CustomSketchConf = CustomSketchConf(
            cmapSize = 10, cmapNo = 2, cmapStart = Some(-10d), cmapEnd = Some(10d),
            counterSize = 10, counterNo = 2
          )
          val sketch0 = Sketch.empty[Double]
          val sketch1O = sketch0.update(0, 1, 1, 2, 3)

          val sketch1 = sketch1O
          val fastPdf = Sketch.fastPdf(sketch1, Double.MaxValue)

          ok
        }

      }

    }

    "sample & samples" in {

      "basic" in {
        val (_, samples0) = NumericDist.normal(0.0, 1).samples(100)
        val sketch0 = Sketch.empty[Double].updateInOrder(samples0)
        val (sketch1, samples1) = sketch0.samples(100)
        val sketch2 = Sketch.empty[Double].updateInOrder(samples1)

        val cond1 = samples1.forall(sample => !sample.isNaN)
        val cond2 = samples1.forall(sample => !sample.isInfinite)
        val cond3 = KLD(sketch1, sketch2) < 1

        if(!cond1 || !cond2) ko(s"samples: $samples1")
        else if(!cond3) ko(s"KLD: ${KLD(sketch1, sketch2)}")
        else ok
      }

      "empty" in {
        val sketch0 = Sketch.empty[Double]
        val (sketch1, sample) = sketch0.sample

        if(sample.isNaN || sample.isInfinite) ko(s"sample: $sample") else ok
      }

    }

    /** End **/

  }

}

object SketchGen {

  def intSketchGen: Gen[Sketch[Int]] = sketchGen[Int]

  def booleanSketchGen: Gen[Sketch[Boolean]] = sketchGen[Boolean]

  def sketchGen[A](implicit measure: Measure[A]): Gen[Sketch[A]] = for {
    conf <- SketchConfGen.sketchConfGen
  } yield Sketch.empty[A](measure, conf)

  def intSketchSample: Option[Sketch[Int]] = intSketchGen.sample

  def booleanSketchSample: Option[Sketch[Boolean]] = booleanSketchGen.sample

}
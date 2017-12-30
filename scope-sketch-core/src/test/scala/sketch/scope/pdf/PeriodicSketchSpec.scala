package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.conf._
import sketch.scope.measure._
import cats.implicits._
import sketch.scope.cmap.{Cmap, DividerCmap}

/**
  * Licensed by Probe Technology, Inc.
  */
class PeriodicSketchSpec extends Specification with ScalaCheck {

  "PeriodicSketch" should {

    "empty" in {
      // construct
      val (cmapSize, cmapNo, cmapMin, cmapMax) = (5, 100, -100, 100)
      val (counterSize, counterNo) = (10, 2)
      implicit val conf: CustomSketchConf = CustomSketchConf(
        CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
        CounterConf(counterSize, counterNo)
      )
      val periodicSketch = PeriodicSketch.empty[Int]
      
      // test
      val strSize = periodicSketch.structures.size
      val cmapSizes = periodicSketch
        .structures
        .map { case (cmap, hcounter) => cmap.size }
      val counterNos = periodicSketch
        .structures
        .map { case (cmap, hcounter) => hcounter.depth }
      val counterSizes = periodicSketch
        .structures
        .map { case (cmap, hcounter) => hcounter.width }

      if(strSize == cmapNo &&
        cmapSizes.forall(_ == cmapSize) &&
        counterNos.forall(_ == counterNo) &&
        counterSizes.forall(_ == counterSize)) ok
      else ko(
        s"strSize: $strSize (expected: $cmapNo), " +
          s"cmapSizes: $cmapSizes (expected: $cmapSize), " +
          s"counterNos: $counterNos (expected: $counterNo), " +
          s"counterSizes: $counterSizes (expected: $counterSize)"
      )
    }

    "periods" in {
      (for {
        periodicSketch <- PeriodicSketchGen.periodicSketchSample
      } yield periodicSketch)
        .fold(ko)(periodicSketch => ok)
    }

    "update" in {

      "check cmap changes for periodic sketch" in {
        val (cmapSize, cmapNo, cmapMin, cmapMax) = (10, 1, 0, 10)
        val (counterSize, counterNo) = (2, 1)
        implicit val conf: CustomSketchConf = CustomSketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0: Sketch[Double] = PeriodicSketch.emptyForPeriod[Double](0, 1)
        val datas = Stream.from(1, 1).take(3)

        val sequencialSketchsO: Option[List[Sketch[Double]]] = datas
          .foldLeft(Option(List(sketch0))){ case (accO, data) => for {
            acc <- accO
            sketch <- acc.lastOption
            utdSketch <- sketch.update(data)
          } yield acc :+ utdSketch }

        val sequencialCmapsO: Option[List[Cmap]] = sequencialSketchsO.flatMap(sketches =>
          sketches.traverse(sketch => sketch.lastCmap)
        )

        val cond = sequencialCmapsO.exists(cmaps => cmaps.sliding(2)
          .map {
            case cmap1 :: cmap2 :: Nil => Some((cmap1, cmap2))
            case _ => None
          }.forall {
            case Some((cmap1: DividerCmap, cmap2: DividerCmap)) => cmap1 != cmap2
            case _ => false
          }
        )

        if(cond) ok else ko(
          sequencialCmapsO.map(cmaps =>
            cmaps.zipWithIndex.map { case (cmap, idx) => s"cmap ${idx+1}: $cmap"}.mkString("\n")
          ).getOrElse("empty cmap")
        )
      }

    }

  }

}

object PeriodicSketchGen {

  def intPeriodicSketchGen: Gen[PeriodicSketch[Int]] = for {
    measure <- MeasureGen.intMeasureGen
    conf <- SketchConfGen.sketchConfGen
  } yield PeriodicSketch.empty(measure, conf)

  def periodicSketchSample: Option[PeriodicSketch[Int]] = intPeriodicSketchGen.sample

}
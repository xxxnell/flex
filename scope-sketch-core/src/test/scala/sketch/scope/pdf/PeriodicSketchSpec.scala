package sketch.scope.pdf

import org.scalacheck.Gen
import org.specs2.mutable._
import org.specs2.ScalaCheck
import sketch.scope.conf.{CmapConf, CounterConf, SketchConf, SketchConfGen}
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
      implicit val conf: SketchConf = SketchConf(
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
        implicit val conf: SketchConf = SketchConf(
          CmapConf.uniform(cmapSize, cmapNo, cmapMin, cmapMax),
          CounterConf(counterSize, counterNo)
        )
        val sketch0: Sketch[Double] = PeriodicSketch.emptyForPeriod[Double](0, 1)
        val datas = Stream.from(1, 1).take(3)

        val sequencialSketchsO: Option[List[Option[Sketch[Double]]]] = datas
          .foldLeft(Option(List(Option(sketch0)))){ case (accO, data) => for {
            acc <- accO
            sketchO <- acc.lastOption
            utdSketchO <- sketchO.traverse(sketch => sketch.update(data))
          } yield acc :+ utdSketchO }

        val sequencialCmapsO: Option[List[Option[Cmap]]] = sequencialSketchsO.map(seqSketches =>
          seqSketches.map(sketchO => for {
            sketch <- sketchO
            recentStr <- sketch.structures.lastOption
          } yield recentStr._1)
        )

        val cond = sequencialCmapsO.exists(sequencialCmaps => sequencialCmaps.sliding(2)
          .map {
            case Some(cmap1) :: Some(cmap2) :: Nil => Some((cmap1, cmap2))
            case _ => None
          }.forall {
            case Some((cmap1: DividerCmap, cmap2: DividerCmap)) => cmap1 == cmap2
            case _ => false
          }
        )

        if(cond) ok else ko(s"cmaps: $sequencialCmapsO")
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
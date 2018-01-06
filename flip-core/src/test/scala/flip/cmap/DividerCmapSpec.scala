package flip.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import flip.hmap.HDim

/**
  * Created by shin-yoonsu on 2017. 9. 19..
  */
class DividerCmapSpec extends Specification with ScalaCheck {

  "DividerCmap" should {

    "divider2IndexingMap" in {
      implicit val dividerGen: Arbitrary[List[Double]] = DividerCmapGen.dividerA

      prop { (divider: (List[Double]) ) =>
        val divider2indexMap = DividerCmap.divider2IndexingMap(divider)
        val headHdimO: Option[HDim] = for {
          firstDivider <- divider.headOption
          idxMap <- divider2indexMap.get(firstDivider)
        } yield idxMap
        val tailHdimO: Option[HDim] = for {
          lastDivider <- divider.lastOption
          idxMap <- divider2indexMap.get(lastDivider)
        } yield idxMap
        val koMsg =
          s"Cannot get the recorded count: " +
            s"divider2indexMap size -> ${divider2indexMap.size}, " +
            s"divider size -> ${divider.size}, " +
            s"headHdimO -> $headHdimO, " +
            s"headHdim -> ${0}, " +
            s"tailHdimO -> $tailHdimO, " +
            s"tailHdim -> ${divider.size-1}"
        if( divider2indexMap.size == divider.size &&
            headHdimO.fold(false)(head => head == 0) &&
            tailHdimO.fold(false)(tail => tail == divider.size-1) ) ok
        else ko(koMsg)
      }.setArbitrary(dividerGen)
    }

    "divider2InverseIndexingMap" in {
      implicit val dividerGen: Arbitrary[List[Double]] = DividerCmapGen.dividerA

      prop { (divider: (List[Double]) ) =>
        val divider2inverseindexMap = DividerCmap.divider2InverseIndexingMap(divider)
        val headDoubleO: Option[Double] = for {
          idxMap <- divider2inverseindexMap.get(0)
        } yield idxMap
        val tailDoubleO: Option[Double] = for {
          idxMap <- divider2inverseindexMap.get(divider.size-1)
        } yield idxMap
        val koMsg =
          s"Cannot get the recorded count: " +
            s"divider2inverseindexMap size -> ${divider2inverseindexMap.size}, " +
            s"divider size -> ${divider.size}, " +
            s"headDoubleO -> $headDoubleO, " +
            s"dividerHead -> ${divider.headOption}, " +
            s"tailDoubleO -> $tailDoubleO, " +
            s"dividerTail -> ${divider.lastOption}"
        if( divider2inverseindexMap.size == divider.size &&
          headDoubleO.fold(false)(head => head == divider.headOption.get) &&
          tailDoubleO.fold(false)(tail => tail == divider.lastOption.get)) ok
        else ko(koMsg)
      }.setArbitrary(dividerGen)
    }

    "bin" in {

      "divider" in {
//        implicit val dividerGen: Arbitrary[List[Double]] = DividerCmapGen.dividerA
//
//        prop { (divider: List[Double]) =>
//          val cmap = DividerCmap(divider)
//          val dividerSize = divider.size
//          val check = (1 until dividerSize)
//            .map(idx => (idx, cmap.bin(idx)))
//            .forall { case (idx, bin) =>
//              bin(0) == divider(idx - 1) && bin(1) == divider(idx)
//            }
//          if(check) ok else ko
//        }.setArbitrary(dividerGen)

        todo
      }

      "uniform" in {
        todo
      }

    }

    "size" in {
      implicit val dividercmapGen: Arbitrary[List[Double]] = DividerCmapGen.dividerA

      prop { (divider: (List[Double]) ) =>
        val dividercmapSize = DividerCmap(divider).size
        val koMsg =
          s"Cannot get the recorded count: " +
            s"dividerCmap size -> $dividercmapSize, " +
            s"divider size -> ${divider.size}"

        if( dividercmapSize - 1 == divider.size) ok
        else ko(koMsg)
      }.setArbitrary(dividercmapGen)
    }

    "range" in {

      "divider" in {
//        implicit val dividerGen: Arbitrary[List[Double]] = DividerCmapGen.dividerA
//
//        prop { (divider: List[Double]) =>
//          val cmap = DividerCmap(divider)
//          val dividerSize = divider.size
//          val check = (1 until dividerSize)
//            .map(idx => (idx, cmap.range(idx)))
//            .forall { case (idx, range) =>
//              range(0) == divider(idx - 1) && range((divider(idx) - divider(idx - 1)).toInt) == divider(idx)
//            }
//          if(check) ok else ko
//        }.setArbitrary(dividerGen)

        todo
      }

      "uniform" in {
        todo
      }

    }

  }

}

object DividerCmapGen {

  def dividerGen: Gen[List[Double]] = for {
    from <- Gen.choose(0, 100)
    to <- Gen.choose(0, 100)
    if from < to
    list = (from to to).toList.map(a => a.toDouble)
  } yield list

  def dividerCmapGen: Gen[DividerCmap] = for {
    dividerList <- DividerCmapGen.dividerGen
    dividercmapGen = DividerCmap(dividerList)
  } yield dividercmapGen

  def dividerA: Arbitrary[List[Double]] = Arbitrary(dividerGen)
  

}
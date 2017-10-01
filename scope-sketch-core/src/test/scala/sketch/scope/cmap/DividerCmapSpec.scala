package sketch.scope.cmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
  * Created by shin-yoonsu on 2017. 9. 19..
  */
class DividerCmapSpec extends Specification with ScalaCheck {

  "DividerCmap" should {

    "divider2IndexingMap" in {
      implicit val dividerGen = DividerCmapGen.dividerA

      prop { (divider: (List[Double]) ) =>
        val divider2indexMap = DividerCmap.divider2IndexingMap(divider)
        val indexmapHead = divider2indexMap.get(divider(0)) == Some(0)
        val indexmapTail = divider2indexMap.get(divider(divider.size-1)) == Some(divider.size-1)
        val koMsg =
          s"Cannot get the recorded count: " +
            s"divider2indexMap size -> ${divider2indexMap.size}, divider size -> ${divider.size}, " +
            s"indexmapHead -> ${divider2indexMap.get(1)}, indexmapTail -> ${divider2indexMap.get(divider.size)}"

        if( divider2indexMap.size == divider.size &&
            indexmapHead &&
            indexmapTail ) ok
        else ko(koMsg)
      }.setArbitrary(dividerGen)
    }

    "divider2InverseIndexingMap" in {
      todo
    }

    "bin" in {
      todo
    }

    "size" in {
      implicit val dividercmapGen = DividerCmapGen.dividerA

      prop { (divider: (List[Double]) ) =>
        val dividercmapSize = DividerCmap(divider).size
        val koMsg =
          s"Cannot get the recorded count: " +
            s"dividerCmap size -> ${dividercmapSize}, divider size -> ${divider.size}"

        if( dividercmapSize - 1 == divider.size) ok
        else ko(koMsg)
      }.setArbitrary(dividercmapGen)
    }

    "range" in {
      todo
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
//    dividercmapGen <- Gen.const(DividerCmap(dividerList))
  } yield dividercmapGen

  def dividerA: Arbitrary[List[Double]] = Arbitrary(dividerGen)
  

}
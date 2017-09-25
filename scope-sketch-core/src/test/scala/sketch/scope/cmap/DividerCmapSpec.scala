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
      todo
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
    from <- Gen.choose(0, 50)
    to <- Gen.choose( 51, 100)
    listGen <- (from to to).toList.map( a => a.toDouble )
  } yield listGen

  def dividerA: Arbitrary[List[Double]] = Arbitrary(dividerGen)
  

}
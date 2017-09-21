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
      todo
    }

    "range" in {
      todo
    }

  }

}

object DividerCmapGen {

  def dividerCmapGen: Gen[DividerCmap] = ???

  def cmapA: Arbitrary[Cmap] = ???

}
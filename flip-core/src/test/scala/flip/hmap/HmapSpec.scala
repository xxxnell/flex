package flip.hmap

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable._
import org.specs2.ScalaCheck
import flip.counter.CDim
import flip.hmap.Hmap

class HmapSpec extends Specification with ScalaCheck {

  "Hmap" should {

    "apply" in {
      implicit val hmapA: Arbitrary[Hmap] = HmapGen.hmapA
      implicit val nontrivialSizeA: Arbitrary[HDim] = HmapGen.nontrivialSizeA

      prop { (hmap: Hmap, hdim: HDim, size: Int) =>
        hmap.apply(hdim, size).fold(ko)(cdim =>
          if(cdim < size && cdim >= 0) ok else ko
        )
      }.setArbitrary1(hmapA).setArbitrary3(nontrivialSizeA)
    }

  }

}

object HmapGen {

  def hmapGen: Gen[Hmap] = for {
    seed <- Arbitrary.arbitrary[Int]
  } yield Hmap(seed)

  def hmapA: Arbitrary[Hmap] = Arbitrary(hmapGen)

  def validSizeGen: Gen[Int] = for {
    size <- Arbitrary.arbitrary[Int]
    if size >= 0
  } yield size

  def validSizeA: Arbitrary[Int] = Arbitrary(validSizeGen)

  def nontrivialSizeGen: Gen[Int] = for {
    size <- validSizeGen
    if size > 0
  } yield size

  def nontrivialSizeA: Arbitrary[Int] = Arbitrary(nontrivialSizeGen)

//  def validHdimGen: Gen[HDim] = for {
//    hdim <- Arbitrary.arbitrary[Int]
//    if hdim >= 0
//  } yield hdim
//
//  def validHdimA: Arbitrary[HDim] = Arbitrary(validHdimGen)

}
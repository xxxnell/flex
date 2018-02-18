package flip.hmap

import cats.data.Kleisli
import flip.{Epi, Mon}
import flip.counter.CDim

/**
  * Hmap, or Hashing Map is hashing function.
  */
trait Hmap {

  /**
    * @param size size of destination counter
    * @return cdim
    * */
  def apply(hdim: HDim, size: Int): Option[CDim]

}

trait HmapOps {

  def kleisli(hmap: Hmap): Epi[(HDim, Int), CDim] = Kleisli[Option, (HDim, Int), CDim] {
    case (hdim, size) =>
      hmap.apply(hdim, size)
  }

}

trait HmapSyntax {

  implicit class HmapSyntaxImpl(hmap: Hmap) {
    def kleisli = ???
  }

}

object Hmap extends HmapOps {

  def apply(seed: Int): Hmap = byteswap32(seed)

  def byteswap32(seed: Int): Byteswap32Hmap = Byteswap32Hmap(seed)

  def identity: IdentityHmap = IdentityHmap()

}

package flex.hmap

import cats.data.Kleisli
import flex.{ Epi, Mon }
import flex.counter.CDim

/**
 * Hmap, or Hashing Map is hashing function.
 */
trait Hmap {

  /**
   * @param size size of destination counter
   * @return cdim
   * */
  def apply(hdim: HDim, size: Int): CDim

}

trait HmapOps {

  def kleisli(hmap: Hmap): Epi[(HDim, Int), CDim] = ???

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

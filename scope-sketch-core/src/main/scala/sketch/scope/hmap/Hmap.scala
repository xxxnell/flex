package sketch.scope.hmap

import cats.data.Kleisli
import sketch.scope.{Epi, Mon}
import sketch.scope.counter.CDim

/**
  * Hashing Map.
  */
trait Hmap {

  /**
    * @param size size of destination counter
    * @return cdim
    * */
  def apply(hdim: HDim, size: Int): Option[CDim]

}

trait HmapOps {

  def kleisli(hmap: Hmap): Epi[(HDim, Int), CDim] = Kleisli[Option, (HDim, Int), CDim]{ case (hdim, size) =>
    hmap.apply(hdim, size)
  }

}

object Hmap extends HmapOps {

  def apply(seed: Int): Hmap = new Byteswap32Hmap(seed)

}

trait HmapSyntax {

  implicit class HmapSyntaxImpl(hmap: Hmap) {
    def kleisli = ???
  }

}
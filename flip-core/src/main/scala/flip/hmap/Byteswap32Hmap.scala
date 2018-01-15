package flip.hmap

import flip.counter.CDim

import scala.util.hashing.byteswap32

class Byteswap32Hmap(seed: Int) extends Hmap {

  val hseed: Int = byteswap32(seed)

  def apply(hdim: HDim, size: Int): Option[CDim] = {
    try {
      if(size > 0) Some(Math.abs(byteswap32(hseed + hdim)) % size) else None
    } catch {
      case _: Exception => None
    }
  }

}

trait Byteswap32HmapOps extends HmapOps {

}

object Byteswap32Hmap extends Byteswap32HmapOps {

  def apply(seed: Int) = new Byteswap32Hmap(seed)

}

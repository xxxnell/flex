package flex.hmap

import flex.counter.CDim

import scala.util.hashing.byteswap32

class Byteswap32Hmap(seed: Int) extends Hmap {

  val hseed: Int = byteswap32(seed)

  def apply(hdim: HDim, size: Int): CDim =
    Math.abs(byteswap32(hseed + hdim)) % size

  override def toString: String = s"Byteswap32Hmap(seed -> $seed)"

}

trait Byteswap32HmapOps extends HmapOps {}

object Byteswap32Hmap extends Byteswap32HmapOps {

  def apply(seed: Int) = new Byteswap32Hmap(seed)

}

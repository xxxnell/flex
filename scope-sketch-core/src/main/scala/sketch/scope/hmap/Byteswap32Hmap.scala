package sketch.scope.hmap

import sketch.scope.counter.CDim

import scala.util.Try
import scala.util.hashing.byteswap32

/**
  * Licensed by Probe Technology, Inc.
  */
class Byteswap32Hmap(seed: Int) extends Hmap {

  val hseed = byteswap32(seed)

  def apply(hdim: HDim, size: Int): Option[CDim] = if(size > 0) {
    Try(Math.abs(byteswap32(hseed + hdim)) % size).toOption
  } else None

}

trait Byteswap32HmapOps extends HmapOps {

}

object Byteswap32Hmap extends Byteswap32HmapOps {

  def apply(seed: Int) = new Byteswap32Hmap(seed)

}

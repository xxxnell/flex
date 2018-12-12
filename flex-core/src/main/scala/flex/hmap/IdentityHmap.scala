package flex.hmap

import flex.counter.CDim

trait IdentityHmap extends Hmap {

  def apply(hdim: HDim, size: Int): CDim = {
    if (hdim >= 0 && hdim < size) hdim else throw new IndexOutOfBoundsException(s"hdim: $hdim, size: $size")
  }

  override def equals(other: Any): Boolean = other.isInstanceOf[IdentityHmap]
  override def hashCode(): HDim = 0
  override def toString: String = "IdentityHmap"
}

trait IdentityHmapOps extends HmapOps

object IdentityHmap extends IdentityHmapOps {

  def apply(): IdentityHmap = new IdentityHmap {}

}

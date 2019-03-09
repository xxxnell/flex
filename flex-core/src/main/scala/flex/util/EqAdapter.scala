package flex.util

case class EqAdapter[A](a: A) { self =>

  override def equals(other: Any): Boolean = other.isInstanceOf[EqAdapter[A]] && {
    other.asInstanceOf[EqAdapter[A]].a.asInstanceOf[AnyRef].eq(self.a.asInstanceOf[AnyRef])
  }

}

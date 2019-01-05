package flex.experiment

package object ops extends TimeOps {

  def mean[A](as: List[A])(implicit frac: Fractional[A]): A =
    if (as.nonEmpty) frac.div(as.sum, frac.fromInt(as.length)) else frac.zero

}

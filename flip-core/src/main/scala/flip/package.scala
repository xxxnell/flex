import cats.data.Kleisli

package object flip extends AllSyntax {

  type Mon[A, B] = Kleisli[Some, A, B]

  type Epi[A, B] = Kleisli[Option, A, B]

  def time[R](block: => R, tag: String = "", display: Boolean = true): R =
    timePrint(block, if (tag.isEmpty) None else Some(tag), display)

  def timePrint[R](block: => R, tag: Option[String], display: Boolean): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val tagPrefixed = tag.map(s => s" $s")
    if (display) println(s"Elapsed time${tagPrefixed.getOrElse("")}: " + (t1 - t0) + " ns")
    result
  }

  def timeCost[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

}

package sketch.scope.cmap

import sketch.scope.hmap.HDim

import scala.collection.immutable.NumericRange

/**
  * Licensed by Probe Technology, Inc.
  */
class UniformCmap(n: Int) extends DividerCmap {

  val divider: List[Double] = {
    val max = BigDecimal(DividerCmap.max)
    val min = BigDecimal(DividerCmap.min)
    val unit = if(n > 0) (max - min) / n else max - min

    (1 to n).toList.map(idx => (min + idx * unit).toDouble)
  }

}

trait UniformCmapOps extends DividerCmapOps[UniformCmap]

object UniformCmap extends UniformCmapOps {

  /**
    * @param n number of pieces
    * */
  def apply(n: Int): UniformCmap = new UniformCmap(n)

}

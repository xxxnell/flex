package flip.benchmark.ops

import flip._
import flip.implicits.NumericDist
import flip.rand.IRng

object SignalOps {

  def normalSignals(sampleNo: Int): List[Double] = {
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(sampleNo)
    datas
  }

  def incrDriftSignals(sampleNo: Int, speed: Double): List[Double] = {
    val underlying = (idx: Int) => NumericDist.normal(idx * speed, 1, IRng(idx))
    (1 to sampleNo).toList.map(idx => underlying(idx).sample._2)
  }

}

package flip.benchmark.ops

import flip._

object SignalOps {

  def normalSignals(sampleNo: Int): List[Double] = {
    val underlying = NumericDist.normal(0.0, 1)
    val (_, datas) = underlying.samples(sampleNo)
    datas
  }

}

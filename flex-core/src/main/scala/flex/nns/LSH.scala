package flex.nns

import org.nd4j.linalg.api.ndarray.INDArray

/**
  * Stable distribution LSH
  * */
trait LSH[V] {

  def hash(x: V): Int

}

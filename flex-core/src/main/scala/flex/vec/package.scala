package flex

import org.nd4j.linalg.api.ndarray.INDArray

package object vec extends VecSyntax with SumVecSyntax {

  type Vec = INDArray

  type SumVec = List[Vec]

}

package flex

import org.nd4j.linalg.api.ndarray.INDArray

package object vec extends VecSyntax with SumVecSyntax with Transform with TransformSyntax with DatasetSyntax {

  type Vec = INDArray

  type SumVec = List[Vec]

}

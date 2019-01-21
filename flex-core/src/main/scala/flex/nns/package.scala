package flex

import flex.vec.{SumVec, Vec}

package object nns {

  type VecLSH = LSH[Vec]

  type SumVecLSH = LSH[SumVec]

  type VecANN = ANN[Vec]

  type SumVecANN = ANN[SumVec]

  object syntax extends ANNSyntax with ParANNSyntax with SumVecANNSyntax with LSHSyntax

}

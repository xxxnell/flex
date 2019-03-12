package flex

import flex.util.{EqAdapter, Memo}
import flex.vec.{SumVec, Vec}

package object nns {

  type LSHMemo = Memo[(EqAdapter[Vec], Int), List[Float]]

  type VecANN = ANN[Vec]

  type SumVecANN = ANN[SumVec]

  object syntax extends ANNSyntax with ParANNSyntax with SumVecANNSyntax

}

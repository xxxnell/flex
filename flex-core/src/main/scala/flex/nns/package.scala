package flex

import flex.util.{EqAdapter, IdentityHashMap, IdentityHashSet, Memo}
import flex.vec.{SumVec, Vec}

package object nns {

  type LSHMemo = Memo[(EqAdapter[Vec], Int), List[Float]]

  type HTable[V] = IdentityHashMap[Int, IdentityHashSet[V]]

  type VTable[V] = IdentityHashMap[V, Int]

  object syntax extends ANNSyntax with ParANNSyntax with SumVecANNSyntax

}

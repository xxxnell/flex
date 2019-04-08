package flex

import flex.util.{EqAdapter, IdentityHashMap, IdentityHashSet, Memo}
import flex.vec.{SumVec, Vec}

package object nns {

  type LSHMemo = Memo[(EqAdapter[Vec], EqAdapter[Vec]), Float]

  object LSHMemo {
    def empty(size: Int): LSHMemo = Memo.empty[(EqAdapter[Vec], EqAdapter[Vec]), Float](size)
  }

  type HTable[V] = Map[Int, IdentityHashSet[V]]

  object HTable {
    def empty[V]: HTable[V] = Map.empty[Int, IdentityHashSet[V]]
  }

  type VTable[V] = IdentityHashMap[V, Int]

  object VTable {
    def empty[V]: VTable[V] = IdentityHashMap.empty[V, Int]
  }

  object syntax extends ANNSyntax with ParANNSyntax with SumVecANNSyntax

}

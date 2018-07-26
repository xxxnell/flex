package flip

import flip.Memo.syntax._
import org.specs2.mutable._
import org.specs2.ScalaCheck

class MemoSpec extends Specification with ScalaCheck {

  "Memo" should {

    "get" in {
      val memo = Memo.empty[Int, Int](10)
      val res = memo.get(1, i => i + 1)
      if(res == 2) ok else ko
    }

    "multiple get for one key" in {
      val memo = Memo.empty[Int, Int](10)
      (0 to 100).foreach(_ => memo.get(1, i => i + 1))

      val cond1 = memo.queue.size == 1
      val cond2 = memo.table.size == 1

      if(!cond1) ko else if(!cond2) ko else ok
    }

    "multiple get for multiple key" in {
      val size = 10
      val memo = Memo.empty[Int, Int](size)
      (0 to 10000000).foreach(i => memo.get(i, j => j + 1))

      val cond1 = memo.queue.size <= size
      val cond2 = memo.table.size <= size

      if(!cond1) ko(s"queue size: ${memo.queue.size}")
      else if(!cond2) ko(s"table size: ${memo.table.size}")
      else ok
    }

  }

}
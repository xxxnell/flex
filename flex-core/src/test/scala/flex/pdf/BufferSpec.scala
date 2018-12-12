package flex.pdf

import org.specs2.mutable._
import org.specs2.ScalaCheck

class BufferSpec extends Specification with ScalaCheck {

  "Buffer" should {

    "construct" in {

      "list" in {
        val size = 100
        val buffer = Buffer(List.fill(size)((1.0, 1.0)))

        if(buffer.dataset.size != size) ko
        else if(buffer.size != size) ko
        else ok
      }

    }

    "append" in todo

    "appends" in todo

    "splitAt" in todo

    "toList" in todo

    "sum" in todo

  }

}
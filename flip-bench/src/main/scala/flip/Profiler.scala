package flip

import flip.cmap.DividerCmap
import flip.pdf.{AdaptiveSketch, Sketch}
import org.openjdk.jol.info.GraphLayout._

object Profiler {

  def serializedMem[A](sketch: Sketch[A]): Long = {
    val buffer =
      parseInstance(sketch.asInstanceOf[AdaptiveSketch[A]].buffer.dataset.map(_._1).asInstanceOf[Object]).totalSize
    val modelCounters =
      parseInstance(sketch.structures.map(_.counter.structures.map(_._2)).asInstanceOf[Object]).totalSize
    val modelCmaps =
      parseInstance(sketch.structures.map(_.cmap.asInstanceOf[DividerCmap].divider).asInstanceOf[Object]).totalSize

    buffer + modelCounters + modelCmaps
  }

}

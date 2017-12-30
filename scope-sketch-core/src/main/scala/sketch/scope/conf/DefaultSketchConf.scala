package sketch.scope.conf

/**
  * Licensed by Probe Technology, Inc.
  */
trait DefaultSketchConf extends SketchConf {

  val cmap: CmapConf = CmapConf.uniform(1000, 10, None, None)

  val counter: CounterConf = CounterConf.apply(200, 2)

}

object DefaultSketchConf extends DefaultSketchConf

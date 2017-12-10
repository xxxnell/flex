package sketch.scope.conf

/**
  * Licensed by Probe Technology, Inc.
  */
object DefaultSketchConf extends SketchConf {
  // cmap
  val cmapSize: Int = 1000
  val cmapNo: Int = 10
  val cmapMin: Double = Double.MinValue
  val cmapMax: Double = Double.MaxValue
  // hcounter
  val counterSize: Int = 200
  val counterNo: Int = 2
}

package flip.conf.pdf

import flip.conf.cmap.CmapConf
import flip.conf.counter.CounterConf

trait CustomPeriodicSketchConf extends CustomSketchConf with PeriodicSketchConf

//object CustomPeriodicSketchConf {
//
//  private case class PeriodicSketchConfImpl( // dist
//                                            delta: Double,
//                                            // sketch
//                                            mixingRatio: Double,
//                                            dataKernelWindow: Double,
//                                            decayFactor: Double,
//                                            cmap: CmapConf,
//                                            counter: CounterConf,
//                                            // sketch: periodic
//                                            startThreshold: Double,
//                                            thresholdPeriod: Double)
//      extends CustomPeriodicSketchConf
//
//  def apply( // dist
//            delta: Double = DefaultAdaPerSketchConf.delta,
//            // sketch
//            mixingRatio: Double = DefaultAdaPerSketchConf.mixingRatio,
//            dataKernelWindow: Double = DefaultAdaPerSketchConf.dataKernelWindow,
//            decayFactor: Double = DefaultAdaPerSketchConf.decayFactor,
//            // sketch: cmap
//            cmapSize: Int = DefaultAdaPerSketchConf.cmap.size,
//            cmapNo: Int = DefaultAdaPerSketchConf.cmap.no,
//            cmapStart: Option[Double] = DefaultAdaPerSketchConf.cmap.start,
//            cmapEnd: Option[Double] = DefaultAdaPerSketchConf.cmap.end,
//            boundaryRatio: Double = DefaultAdaPerSketchConf.cmap.boundaryRatio,
//            // sketch: counter
//            counterSize: Int = DefaultAdaPerSketchConf.counter.size,
//            counterNo: Int = DefaultAdaPerSketchConf.counter.no,
//            // sketch: periodic
//            startThreshold: Double = DefaultAdaPerSketchConf.startThreshold,
//            thresholdPeriod: Double = DefaultAdaPerSketchConf.thresholdPeriod): CustomPeriodicSketchConf =
//    bare(
//      delta,
//      mixingRatio,
//      dataKernelWindow,
//      decayFactor,
//      CmapConf.uniformEqualize(cmapSize, cmapNo, cmapStart, cmapEnd, boundaryRatio),
//      CounterConf(counterSize, counterNo),
//      startThreshold,
//      thresholdPeriod
//    )
//
//  def bare( // dist
//           delta: Double,
//           // sketch
//           mixingRatio: Double,
//           dataKernelWindow: Double,
//           decayFactor: Double,
//           cmap: CmapConf,
//           counter: CounterConf,
//           // sketch: periodic
//           startThreshold: Double,
//           thresholdPeriod: Double): CustomPeriodicSketchConf =
//    PeriodicSketchConfImpl(
//      delta,
//      mixingRatio,
//      dataKernelWindow,
//      decayFactor,
//      cmap,
//      counter,
//      startThreshold,
//      thresholdPeriod
//    )
//
//}

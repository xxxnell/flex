package flip.experiment.ops

import flip.pdf.{Dist, SamplingDist, Sketch, SmoothDist}

object ComparisonOps {

  val defaultMinCutoff: Double = -10e10

  val defaultMaxCutoff: Double = 10e10

  def identicalDomain[A, B](dist: Dist[A], sketch: Sketch[A], compare: (SamplingDist[A], Sketch[A]) => B): B =
    identicalDomainForCutoff(dist, defaultMinCutoff, defaultMaxCutoff, sketch, compare)

  def identicalDomainForCutoff[A, B](dist: Dist[A],
                                     minCutoff: Double,
                                     maxCutoff: Double,
                                     sketch: Sketch[A],
                                     compare: (SamplingDist[A], Sketch[A]) => B): B = {
    val sampling = dist.sampling(sketch)
    val filtered = sampling.filter { range =>
      range > minCutoff && range < maxCutoff
    }
    compare(filtered, sketch)
  }

  def uniformDomain[A, B](dist: Dist[A],
                          start: A,
                          end: A,
                          no: Int,
                          sketch: Sketch[A],
                          compare: (SamplingDist[A], Sketch[A]) => B): B =
    uniformDomainForCutoff(dist, start, end, no, defaultMinCutoff, defaultMaxCutoff, sketch, compare)

  def uniformDomainForCutoff[A, B](dist: Dist[A],
                                   start: A,
                                   end: A,
                                   no: Int,
                                   minCutoff: Double,
                                   maxCutoff: Double,
                                   sketch: Sketch[A],
                                   compare: (SamplingDist[A], Sketch[A]) => B): B = {
    val sampling = dist.uniformSampling(start, end, no)
    val filtered = sampling.filter { range =>
      range > minCutoff && range < maxCutoff
    }
    compare(filtered, sketch)
  }

}

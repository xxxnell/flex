package flip.experiment.ops

import flip.pdf.{SamplingDist, Sketch, SmoothDist}

object ComparisonOps {

  val defaultMinCutoff: Double = -10e10

  val defaultMaxCutoff: Double = 10e10

  def identicalDomain[A, B](dist: SmoothDist[A], sketch: Sketch[A],
                            compare: (SamplingDist[A], Sketch[A]) => Option[B]): Option[B] =
    identicalDomainForCutoff(dist, defaultMinCutoff, defaultMaxCutoff, sketch, compare)

  def identicalDomainForCutoff[A, B](dist: SmoothDist[A], minCutoff: Double, maxCutoff: Double,
                                     sketch: Sketch[A],
                                     compare: (SamplingDist[A], Sketch[A]) => Option[B]): Option[B] = for {
    sampling <- dist.sampling(sketch)
    filtered = sampling.filter { range => range > minCutoff && range < maxCutoff }
    ff <- compare(filtered, sketch)
  } yield ff

  def uniformDomain[A, B](dist: SmoothDist[A], start: A, end: A, no: Int,
                          sketch: Sketch[A],
                          compare: (SamplingDist[A], Sketch[A]) => Option[B]): Option[B] =
    uniformDomainForCutoff(dist, start, end, no, defaultMinCutoff, defaultMaxCutoff, sketch, compare)

  def uniformDomainForCutoff[A, B](dist: SmoothDist[A], start: A, end: A, no: Int,
                                   minCutoff: Double, maxCutoff: Double,
                                   sketch: Sketch[A],
                                   compare: (SamplingDist[A], Sketch[A]) => Option[B]): Option[B] = for {
    sampling <- dist.uniformSampling(start, end, no)
    filtered = sampling.filter { range => range > minCutoff && range < maxCutoff }
    compare <- compare(filtered, sketch)
  } yield compare

}

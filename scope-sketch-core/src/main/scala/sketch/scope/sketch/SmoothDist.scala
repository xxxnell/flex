package sketch.scope.sketch

import scala.language.higherKinds

/**
  * Licensed by Probe Technology, Inc.
  */
trait SmoothDist[A] extends Dist[A]

trait SmoothDistPropOps[SD[_]<:SmoothDist[_]] extends DistPropOps[SD]

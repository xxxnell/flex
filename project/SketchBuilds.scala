package sketch.scope

import sbt.Keys._
import Dependencies._
import Resolvers._

object SketchBuilds {

  val buildSettings = Seq(
    version := s"1.0$snapshot",
    scalaVersion := "2.12.1"
  )

  val defaultSettings = Seq(
    resolvers ++= typesafeRepo
  )

}

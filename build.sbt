import sbt.Keys._
import flip.Dependencies._
import flip.FlipBuilds

name := "flip"

lazy val root = Project(id = "flip", base = file("."))
  .aggregate(flipCore, flipBench)

lazy val flipCore = flipModule("flip-core")

lazy val flipBench = flipModule("flip-bench")
  .dependsOn(flipCore)

def flipModule(name: String): Project =
  Project(id = name, base = file(name))
    .settings(FlipBuilds.buildSettings)

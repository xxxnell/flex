import sbt.Keys._
import flip.Dependencies._
import flip.FlipBuilds
import flip.Releases

name := "flip"

lazy val root = project
  .in(file("."))
  .settings(moduleName := "root")
  .settings(Releases.noPublishSettings)
  .aggregate(flipCore, flipBench)

lazy val flipCore = flipModule("flip-core")
  .settings(moduleName := "flip", name := "Flip core")
  .settings(Releases.publishSettings)

lazy val flipBench = flipModule("flip-bench")
  .settings(moduleName := "flip-bench", name := "Flip benchmarks")
  .settings(Releases.noPublishSettings)
  .dependsOn(flipCore)

def flipModule(name: String): Project =
  Project(id = name, base = file(name))
    .settings(FlipBuilds.buildSettings)

import sbt.Keys._
import flip.FlipBuilds
import flip.Releases
import flip.Tasks

name := "flip"

lazy val root = project
  .in(file("."))
  .settings(moduleName := "root")
  .settings(Releases.publishSettings)
  .settings(Releases.noPublishSettings)
  .settings(Tasks.taskSettings(flipBench))
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

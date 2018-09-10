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
  .settings(Tasks.benchTaskSettings(flipBench))
  .aggregate(flipCore, flipChain, flipBench)

lazy val flipCore = flipModule("flip-core")
  .settings(moduleName := "flip", name := "Flip core")
  .settings(Releases.publishSettings)

lazy val flipChain = flipModule("flip-chain")
  .settings(moduleName := "flip", name := "Flip core")
  .settings(Releases.publishSettings)
  .dependsOn(flipCore)

lazy val flipBench = flipModule("flip-bench")
  .settings(moduleName := "flip-bench", name := "Flip benchmarks")
  .settings(Releases.noPublishSettings)
  .dependsOn(flipCore, flipChain)

def flipModule(name: String): Project =
  Project(id = name, base = file(name))
    .settings(FlipBuilds.buildSettings)

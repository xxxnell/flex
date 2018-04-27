package flip

import sbt.Keys._
import sbt.{Def, _}
import complete.DefaultParsers._
import pl.project13.scala.sbt.JmhPlugin.JmhKeys._

object Tasks {

  def benchTaskSettings(benchProject: Project) = Seq(
    benchmarkTaskSetting(benchProject),
    experimentTaskSetting(benchProject)
  )

  // benchmark

  lazy val benchmark = inputKey[Unit]("Execute benchmarks")

  def benchmarkTaskSetting(project: Project) = benchmark := {
    (run in (project, Jmh)).evaluated
  }

  // experiment

  lazy val experiment = inputKey[Unit]("Execute experiments")

  def experimentTaskSetting(project: Project) = experiment := {
    val mains = (discoveredMainClasses in (project, Compile)).value
    val r = (runner in run).value
    val classpath: Seq[File] = (fullClasspath in (project, Compile)).value.files
    val logger = streams.value.log

    val args: Seq[String] = spaceDelimited("<arg>").parsed
    val targets = if (args.nonEmpty) {
      args
        .map(arg => mains.filter(main => arg.r.findFirstIn(main).nonEmpty))
        .foldLeft(Seq.empty[String])((acc, targets) => acc ++ targets)
    } else mains.filter(main => main.startsWith("flip.experiment"))

    targets.foreach(main => r.run(main, classpath, Seq(), logger))
  }

}

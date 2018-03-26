package flip

import sbt._
import sbt.Keys._
import complete.DefaultParsers._
import complete.Parser

object Tasks {

  lazy val experiment = inputKey[Unit]("Execute all experiments")

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

  def taskSettings(expProject: Project) = Seq(
    experimentTaskSetting(expProject)
  )

}

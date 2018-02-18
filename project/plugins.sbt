logLevel := Level.Warn

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// build
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.4.0")

// benckmark
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.3")

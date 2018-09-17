logLevel := Level.Warn

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("io.get-coursier" % "sbt-coursier"   % "1.0.1")
addSbtPlugin("com.47deg"       % "sbt-microsites" % "0.7.23")

// build
addSbtPlugin("org.scoverage"     % "sbt-scoverage" % "1.5.1")
addSbtPlugin("com.geirsson"      % "sbt-scalafmt"  % "1.4.0")
addSbtPlugin("com.github.gseitz" % "sbt-release"   % "1.0.7")
addSbtPlugin("org.foundweekends" % "sbt-bintray"   % "0.5.3")

// benckmark
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")

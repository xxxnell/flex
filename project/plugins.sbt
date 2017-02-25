logLevel := Level.Warn

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// build
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.3")

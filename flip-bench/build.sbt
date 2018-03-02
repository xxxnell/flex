import flip._

enablePlugins(JmhPlugin)
mainClass in (Jmh, run) := Some("flip.benchmark.BenchApp")
scalacOptions += "-Ylog-classpath"

FlipBuilds.defaultSettings
Dependencies.bench
Releases.noPublishSettings

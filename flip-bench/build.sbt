import flip._

enablePlugins(JmhPlugin)
mainClass in (Jmh, run) := Some("flip.benchmark.BenchApp")

FlipBuilds.defaultSettings
Dependencies.bench
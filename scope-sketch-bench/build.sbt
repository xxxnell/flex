import sketch.scope._

enablePlugins(JmhPlugin)
mainClass in (Jmh, run) := Some("sketch.scope.benchmark.BenchApp")

SketchBuilds.defaultSettings
Dependencies.bench
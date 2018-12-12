import flex._

enablePlugins(JmhPlugin)
mainClass in (Jmh, run) := Some("flex.benchmark.BenchApp")

FlexBuilds.defaultSettings
Dependencies.bench
Releases.noPublishSettings

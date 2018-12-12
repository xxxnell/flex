import flex._

enablePlugins(JmhPlugin)
mainClass in (Jmh, run) := Some("flex.benchmark.BenchApp")

flexBuilds.defaultSettings
Dependencies.bench
Releases.noPublishSettings

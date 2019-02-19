---
layout: docs
title:  "Documentation"
section: "docs"
permalink: /docs/
position: 20
---

# Getting Started

Flex is published to Maven Central and built for Scala 2.12, so you can add the following to your `build.sbt`:

``` scala
libraryDependencies ++= Seq(
  "com.xxxnell" %% "flex-core",
  "com.xxxnell" %% "flex-chain"
).map(_ % "0.0.5")
```

Then, you need to `import` the context of Chain.

``` scala
import flex.implicits._
import flex.chain.implicits._
```

# Modules

* **core**: Fast and lightweight probability tools for a dataset and a data stream.
* **chain**: Probabilistic deep learning library.

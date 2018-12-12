---
layout: docs
title:  "Documentation"
section: "docs"
permalink: /docs/
position: 20
---

# Quick Start

It is usual practice to analyze statistical characteristics of datasets and data streams with batch processing. However, this method is slow and does not analyze data streams in real-time. Instead, flex proposes the method that builds a statistical model (represented by a [PDF](https://en.wikipedia.org/wiki/Probability_density_function)) from datasets and data streams and then analyzes this model. This model is updated as the data stream changes.

<p align="center">
<img src="/flex/img/diagrams/concepts/scheme.png" alt="infographic" style="max-width: 600px; width: 100%;"/>
</p>

## Getting Started

flex is published to Maven Central and built for Scala 2.12, so you can add the following to your build.sbt:

``` scala
libraryDependencies += "com.xxxnell" %% "flex" % "0.0.4"
```

Then, you need to `import` the context of flex.

``` scala
import flex.implicits._
```

## Summarizing a Data Stream

First, let's create a data stream by randomly extracting 100 data from the standard normal distribution $\mathcal {N} (0, 1)$ with an mean of 0 and a variance of 1.

``` scala
val underlying = NumericDist.normal(0.0, 1.0)
val (_, samples) = underlying.samples(100)
```

Now, construct a model that reflects the data stream using `Sketch` data structure. The code below shows that creates an empty model (called `sketch0`) with no data recorded, and records the data stream that we created earlier. After the data stream is recorded, the `Sketch` model is updated with the model that reflects the data stream (called `sketch1`).

``` scala
val sketch0 = Sketch.empty[Double]
val sketch1 = samples.foldLeft(sketch0) {
  case (sketch, sample) => sketch.update(sample)
}
```

Then, get various statistics from `sketch1`, a model that represents the previously obtained data stream. The code below shows an operation that takes four different statistical features from `sketch1`. First, we get the probability of a certain interval from `sketch1`. Second, we get the median of `sketch1`. Third, we extract the sample from `sketch1`. Last, we measure the [Kullback-Leibler divergence](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence) between `sketch1` and the *underlying distribution*--i.e. standard normal distribution.

``` scala
// Estimated Pr(0.0 ≤ x ≤ 1.0)
val prob = sketch1.probability(0.0, 1.0)
// Estimated median
val median = sketch1.median
// Sample from sketch
val sample = sketch1.sample._2
// KL-divergence
val kld = KLD(underlying, sketch1)
```

In addition to the various operations introduced here, `Sketch` can be used to perform online statistical processing on various datasets and data streams.


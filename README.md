# Flip 🎲

[![Build Status](https://travis-ci.org/xxxnell/flip.svg?branch=master)](https://travis-ci.org/xxxnell/flip)
[![codecov](https://codecov.io/gh/xxxnell/flip/branch/master/graph/badge.svg)](https://codecov.io/gh/xxxnell/flip)
[![Waffle.io - Columns and their card count](https://badge.waffle.io/xxxnell/flip.svg?columns=to%20do)](https://waffle.io/xxxnell/flip)
[![Latest version](https://index.scala-lang.org/xxxnell/flip/flip/latest.svg)](https://index.scala-lang.org/xxxnell/flip/flip)


*Flip* is *F*ast, *L*ightweight pure-functional library for *I*nformation theory and *P*robability distribution. *Flip* aims to extract and process statistical features of the input data stream in a short time using only small memory. It has the following features:

* Quickly estimate and summarize probability distribution for random variable stream using `Sketch`
* Combine several probability distributions by probability monad
* Generate random variables from many predefined and estimated probability disributions 
* Measure similarity between two probability distribution using Kullback–Leibler divergence


## Getting Started

*Flip* is published to Maven Central and built for Scala 2.12, so you can add the following to your `build.sbt`:

``` scala
libraryDependencies += "com.xxxnell" %% "flip" % "0.0.3"
```


## Summarizing Random Variable Stream using `Sketch`

`Sketch` is the probablistic data structure that quickly measures the probalility density for the real number random variable data stream with limited memory without prior knowledge. Simply put, `Sketch` is a special histogram in which the width of each bin is adaptively adjusted to the input data stream, unlike conventional histograms, which require the user to specify the width and start/end point of the bin. It follows the change of probability distribution, and adapts to the sudden/gradual [concept drift](https://en.wikipedia.org/wiki/Concept_drift). Also, more than two `Sketch` can be combined in monadic way. This is what we call the probability monad in functional programming. `Sketch` is a better alternative to [kernel density estimation](https://en.wikipedia.org/wiki/Kernel_density_estimation) and [histogram](https://en.wikipedia.org/wiki/Histogram) in most cases.

Here is an example of how `Sketch` estimates the density using the dataset sampled from the standard normal distribution.

``` scala 
import flip.implicits._

// get 100 random variables from standard normal distribution
val underlying = NumericDist.normal(0.0, 1.0)
val (_, samples) = underlying.samples(100)

// update samples to sketch
val sketch0 = Sketch.empty[Double]
val sketch1 = samples.foldLeft(sketch0) {
  case (sketch, sample) => sketch.update(sample)
}

// analyze sketch
println(
  s"Estimated Pr(0.0 ≤ x ≤ 1.0): ${sketch1.probability(0.0, 1.0)}, " +
    s"Expected Pr(0.0 ≤ x ≤ 1.0): ${underlying.probability(0.0, 1.0)}\n" +
    s"Estimated median: ${sketch1.median}, expected median: 0.0 \n" +
    s"Sample from sketch: ${sketch1.sample._2} \n" +
    s"KL-divergence: ${KLD(underlying, sketch1)}"
)
```


### The case of bimodal distribution

Here is an experiment result for a bimodal probabability density function consisting of two standard normal distributions centered at -2 and 2.

![animated bimodal](./flip-docs/resources/experiments/basic-bimodal-histo.gif)

In this figure, the dashed orange line is the expected underlying probability distribution, and the blue bar is the probability distribution that `Sketch` estimates. `Sketch` assumes an initial bin with a uniform width, and estimates the first optimal bin at the update count of 50. Then `Sketch` estimates new bins every 100 data updates, for example, 50, 150, 250, and so on.


### The case of concept drift

`Sketch` also adapts to any types of concept drift successfully. Here is an experiment result under the situation where the distribution that `Sketch` is supposed to estimate is gradually changing over time. The underlying distribution starts to change when the update count come to 300 and moves by +0.01 per one update count. `Sketch` is good at predicting this moving distribution, although there is some lag. Also this lag can be reduced by adjusting the sensitivity to new data.

![animated gradual concept drift](./flip-docs/resources/experiments/gradual-cd-normal-histo.gif)

In all of these experiments, I did not provide any prior knowledge to predict the underlying distirbution accurately. It works precisely with the default configuration. For more example, see the [experiment](./flip-docs/experiment.md) documentation. If you want to learn how to use `Sketch` in a real world, see the [code for these experiments](./flip-bench/src/main/scala/flip/experiment).


## Contributing

Contributions are always welcome. Any kind of contribution, such as writing a unit test, documentation, bug fix, or implementing [the density estimation algorithm of `Sketch`](./flip-docs/algorithm.md) in another language, is helpful. If you need some help, please contact me via [email](mailto:xxxxxnell@gmail.com) or [twitter](https://twitter.com/xxxnell).

The `master` branch of this repository contains the latest stable release of *Flip*. In general, pull requests should be submitted from a separate `feature` branch starting from the `develop` branch. 

Fo more detail, see the [contributing](./CONTRIBUTING.md) documentation.


## License

All code of *Flip* is available to you under the [MIT license](./LICENSE). 

Copyright the maintainers.


# Chain

[![Build Status](https://travis-ci.org/xxxnell/flex.svg?branch=master)](https://travis-ci.org/xxxnell/flex)
[![codecov](https://codecov.io/gh/xxxnell/flex/branch/master/graph/badge.svg)](https://codecov.io/gh/xxxnell/flex)
[![Waffle.io - Columns and their card count](https://badge.waffle.io/xxxnell/flex.svg?columns=to%20do)](https://waffle.io/xxxnell/flex)
[![Latest version](https://index.scala-lang.org/xxxnell/flex/flex/latest.svg)](https://index.scala-lang.org/xxxnell/flex/flex)


Chain is a probabilistic deep learning library for data streams. Today, neural networks have been widely used for solving problems in many areas. However, classical neural networks have some limitations when you want to include uncertainties in the model. For example, suppose that input data and training data contain a lot of noise. If you need to detect whether the data contains false-positive or false-negative, the model should represent how reliable the input and the output are. To deal with this issue, probabilistic deep learning, also known as the Bayesian neural network, can be used. It is a way to treat both input and output as a probability distribution and it is one of the best approaches to represent uncertainties. However, the Bayesian neural network is so computationally slow that it cannot be readily applied to the real-world problems. Chain is fast enough to make it possible to apply the Bayesian neural network to the real-world problems. It has the following features:

* Feedforward propagation
* Fast and lightweight probability tools for a dataset and a data stream
	* Pre-defined numeric probabilities (e.g. normal distribution, log-normal distribution)
	* Density estimation for high-dimensional datasets and data streams


## Getting Started

**WIP**. Chain is published to Maven Central and built for Scala 2.12, so you can add the following to your `build.sbt`:

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


## Building a Model

We will use 3 hiddel layers with 10 neurons each.

``` scala
val (d0, l1, l2, l3) = (784, 10, 10, 1)
val model = Complex.empty
  .add(Nd4j.zeros(d0), Nd4j.ones(d0))
  .add(Nd4j.zeros(d0 * l1), Nd4j.ones(d0 * l1))
  .add(Nd4j.zeros(l1 * l2), Nd4j.ones(l1 * l2))
  .add(Nd4j.zeros(l2 * l3), Nd4j.ones(l2 * l3))
  .map { case x1 :: z1 :: rem => x1.mmul(z1.reshape(d0, l1)).tanh :: rem }
  .map { case h1 :: z2 :: rem => h1.mmul(z2.reshape(l1, l2)).tanh :: rem }
  .map { case h2 :: z3 :: rem => h2.mmul(z3.reshape(l2, l3)) :: rem }
```

First, construct an empty model using `Complex.empty`. Second, `add` the variables to be used for this neural network. Here, a prior probabilities of these variables are normal distributions with a mean of zero and a variance of one. Third, define a transformation of each layers using `map` operation. In this example, `tanh` was used as the activator.


## Contributing

Contributions are always welcome. Any kind of contribution, such as writing a unit test, documentation, bug fix, or implementing the algorithm of Chain in another language, is helpful. It is also possible to make academic collaboration works. If you need some help, please contact me via [email](mailto:xxxxxnell@gmail.com) or [twitter](https://twitter.com/xxxnell).

The `master` branch of this repository contains the latest stable release of Chain. In general, pull requests should be submitted from a separate `feature` branch starting from the `develop` branch. 

Fo more detail, see the contributing documentation.


## License

All code of Chain is available to you under the MIT license. 

Copyright the maintainers.


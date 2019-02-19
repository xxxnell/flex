# Chain

[![Build Status](https://travis-ci.org/xxxnell/flex.svg?branch=master)](https://travis-ci.org/xxxnell/flex)
[![codecov](https://codecov.io/gh/xxxnell/flex/branch/master/graph/badge.svg)](https://codecov.io/gh/xxxnell/flex)
[![Waffle.io - Columns and their card count](https://badge.waffle.io/xxxnell/flex.svg?columns=to%20do)](https://waffle.io/xxxnell/flex)
[![Latest version](https://index.scala-lang.org/xxxnell/flex/flex/latest.svg)](https://index.scala-lang.org/xxxnell/flex/flex)


Chain is a probabilistic deep learning library for data streams. It has the following features:

* **Fast**. Chain provides probabilistic deep learning that is fast enough to solve the real-world problems.
* **Typesafe and Functional**. Types and pure functions make the code easy to understand and maintain.
*  **Easy**. You can program with a minimal knowledge of probability theory.

Today, neural networks have been widely used for solving problems in many areas. However, classical neural networks have some limitations when you want to include uncertainties in the model. For example, suppose that input data and training data contain a lot of noise. If you need to detect whether the data contains false-positive or false-negative, the model should represent how reliable the input and the output are. To deal with this issue, probabilistic deep learning, also known as the Bayesian neural network, can be used. It is a way to treat both input and output as a probability distribution and it is one of the best approaches to represent uncertainties. However, the Bayesian neural network is so computationally slow that it cannot be readily applied to the real-world problems. Chain is fast enough to make it possible to apply the Bayesian neural network to the real-world problems. 


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
val (kin, kout) = (20, 10)
val (l0, l1, l2, l3) = (784, 10, 10, 1)
val (k0, k1, k2, k3) = (20, 20, 20, 20)
val model0 = Complex
  .empty(kin, kout)
  .addStd(l0 -> k0, l0 * l1 -> k1, l1 * l2 -> k2, l2 * l3 -> k3)
  .map { case x1 :: z1 :: rem => z1.reshape(l1, l0).mmul(x1).tanh :: rem }
  .map { case h1 :: z2 :: rem => z2.reshape(l2, l1).mmul(h1).tanh :: rem }
  .map { case h2 :: z3 :: rem => z3.reshape(l3, l2).mmul(h2) :: rem }
```

First, construct an empty model using `Complex.empty`. Second, `add` the variables to be used for this neural network. Here, a prior probabilities of these variables are standard normal distributions with a mean of zero and a variance of one. Third, define a transformation of each layers using `map` operation. In this example, `tanh` was used as the activator.


## Contributing

Contributions are always welcome. Any kind of contribution, such as writing a unit test, documentation, bug fix, or implementing the algorithm of Chain in another language, is helpful. It is also possible to make academic collaboration works. If you need some help, please contact me via [email](mailto:xxxxxnell@gmail.com) or [twitter](https://twitter.com/xxxnell).

The `master` branch of this repository contains the latest stable release of Chain. In general, pull requests should be submitted from a separate `feature` branch starting from the `develop` branch. 

Fo more detail, see the contributing documentation.


## License

All code of Chain is available to you under the MIT license. 

Copyright the maintainers.


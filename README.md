# Flip 

[![Build Status](https://travis-ci.org/xxxnell/flip.svg?branch=master)](https://travis-ci.org/xxxnell/flip)
[![Waffle.io - Columns and their card count](https://badge.waffle.io/xxxnell/flip.svg?columns=to%20do)](https://waffle.io/xxxnell/flip)

## Overview

*Flip* is Fast, Lightweight pure-functional library for Information theory and Probability distribution. It has the following features:

* Summarizing probability distribution for random variable stream using `Sketch`
* Compose probability distributions with monad
* Generate random variables from probability disributions 
* Measure similarity between two probability distribution using Kullback–Leibler divergence


## Getting Started

*Flip* is currently available for Scala 2.12.

TODO


## Summarizing Random Variable Stream with `Sketch`

`Sketch` is the probablistic data structure that quickly measures the probalility density for the real number random variable data stream with limited memory. It follows the change of probability distribution, and adapts to the sudden concept drift. Also, more then two `Sketch` can be composed in monadic way. `Sketch` is better alternative to kernel density estimation and histogram in most cases. 

At the same time, it is generalization of the [Count-min sketch](https://en.wikipedia.org/wiki/Count%E2%80%93min_sketch) data structure, the frequency counter for finite and countable elements.

``` scala 
import flip._

// get 100 random variables from standard normal distribution 
val (_, samples) = NumericDist.normal(0.0, 1.0).samples(100)

// update samples to sketch
val sketch0 = Sketch.empty[Double]
val utdSketch = samples.foldLeft(sketch0){ case (sketch, sample) => 
  sketch.update(sample).getOrElse(sketch) 
}

// get probability, that is, integral pdf from 0 to 1.0
println(utdSketch.probability(0, 1)) 
```

Here are some results for typical probability distributions: normal, log-normal, and pareto.

![density estimation for normal distribution](./flip-docs/resources/basic-normal.pdf)
![density estimation for log-normal distribution](./flip-docs/resources/basic-lognormal.pdf)
![density estimation for pareto distribution](./flip-docs/resources/basic-pareto.pdf)

For more example, see [`flip-bench/src/main/scala/flip/experiment`](https://github.com/xxxnell/flip/tree/develop/flip-bench/src/main/scala/flip/experiment).


## Contributions

Contributions are always welcome. 


## License

All code of *Flip*  is available to you under the MIT license. 

Copyright the maintainers.


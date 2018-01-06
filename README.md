# Flip 

[![Build Status](https://travis-ci.org/xxxnell/flip.svg?branch=master)](https://travis-ci.org/xxxnell/flip)

## Overview

*Flip* is pure-functional Fast, Lightweight library for Information theory and Probability distribution. It has the following features:

* Summarizing probability distribution for random variable stream using `Sketch`
* Probability distribution composition with monad
* Generate random variables from probability disributions 
* Measure similarity between two probability distribution using Kullback–Leibler divergence

## Getting Started

Flip is currently available for Scala 2.12.

TODO

## Summarizing Random Variable Stream with `Sketch`

`Sketch` is the probablistic data structure that measures the probalility for the real number random variable data stream with limited memory. It follows the change of probability distribution, and adapts to the sudden concept drift. Also, more then two `Sketch` can be composed in monadic way.
At the same time, it is generalization of the [Count-Min Sketch](https://en.wikipedia.org/wiki/Count%E2%80%93min_sketch) data structure, that is the frequency counter for finite and countable elements.

``` scala 
import flip._

// get 100 random variables from standard normal distribution 
val (_, samples) = Dist.normal(0d, 1).samples(100)

// update samples to sketch
val sketch0 = Sketch.empty[Double]
val utdSketch = samples.foldLeft(sketch0){ case (sketch, sample) => sketch.update(sample).getOrElse(sketch) }

// get probability, that is, integral pdf from 0 to 1.0
println(utdSketch.probability(0, 1))
```

For more example, see [`flip-bench/src/main/scala/flip/experiment`](https://github.com/xxxnell/flip/tree/develop/flip-bench/src/main/scala/flip/experiment).


## Contributions

Contributions are always welcome. 


# License

All code of *Flip*  is available to you under the MIT license. 
Copyright the maintainers.


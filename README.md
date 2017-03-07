# Scope Sketch 

[![Build Status](https://travis-ci.org/namukpark/scope-sketch.svg?branch=master)](https://travis-ci.org/namukpark/scope-sketch)
[![codecov](https://codecov.io/gh/namukpark/scope-sketch/branch/master/graph/badge.svg)](https://codecov.io/gh/namukpark/scope-sketch)


Scope Sketch is the probablistic data structure that measures the frequency - or probalility -  of the data stream composed of measurable elements. At the same time, it is generalization of the [Count-Min Sketch](https://en.wikipedia.org/wiki/Count%E2%80%93min_sketch) data structure, that is the frequency counter for finite and countable elements.


# Getting Started 

``` scala 
import sketch.scope._

// set uniform distribution
val dist = 0.0 until 1.0 by 0.01

// update the distribution
val sketch = dist.foldLeft(Sketch[Float])((sketch, data) => sketch.update(data)) 

// get the probability, that is, integral from 0.1 to 0.2 of pdf
println(sketch.probability(0.1, 0.2))
```


# Dependencies

## Maven Configuration

``` xml
```


## sbt Configuration

``` scala
```

# Documentation

* [Scaladoc]()


# Bugs and Feedback




# LICENSE


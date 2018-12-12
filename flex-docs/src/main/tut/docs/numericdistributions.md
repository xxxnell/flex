---
layout: docs
title:  "Numeric Distributions"
section: "docs"
---

# Numeric Distributions

WIP

## Dirac Delta Function

[Dirac-delta distribution](https://en.m.wikipedia.org/wiki/Dirac_delta_function)

``` scala
val mean = 0.0
val dist = DeltaDist(mean)
```

## Normal Distribution

[Normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)

``` scala
val mean = 0.0
val variance = 1.0
val dist = NormalDist(mean, variance)
```

## Log-normal Distribution

[Log-normal distribution](https://en.wikipedia.org/wiki/Log-normal_distribution)

``` scala
val scale = 0.0
val shape = 1.0
LogNormalDist(scale, shape)
```

## Pareto Distribution	

[Pareto distribution](https://en.wikipedia.org/wiki/Pareto_distribution)

``` scala
val scale = 0.0
val shape = 1.0
ParetoDist(scale, shape)
```

## Uniform Distribution	

[Uniform distribution](https://en.wikipedia.org/wiki/Uniform_distribution_(continuous))

``` scala
val scale = 0.0
val width = 1.0
UniformDist(scale, width)
```
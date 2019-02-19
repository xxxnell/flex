---
layout: docs
title:  "Model"
section: "docs"
---

# Model

## Get Started

WIP

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

## Training a Model

WIP

``` scala
val mnistA = Dataset.mnistTrain.runSyncUnsafe(10.seconds)
val mnistB = Dataset.mnistTest.runSyncUnsafe(10.seconds)

val model1 = model0.train(mnistA)
```

## Testing a Model

WIP

``` scala
model1.evaluate(mnistB)
```

# Benchmarks

*Flip* claims a fast and lightweight library, so reducing computation cost and memory consumption is one of the most important goals. As shown in the benchmark below, *Flip* is not only easy to use, but also the best performance library for statistical processing of dataset as a performance aspect.

All benchmarks are using version [v0.0.2](https://github.com/xxxnell/flip/tree/v0.0.2).


## `Sketch` time performance 

The table below shows the result of execution time benchmark for the primary functions of `Sketch` in nanosecond (ns) units for cases where queueSize is 0 and 50. The benchmark environment is the Macbook Pro 2016 with 2.9 GHz Intel Core i5 and 16 GB Memory. I used [jmh] (http://openjdk.java.net/projects/code-tools/jmh/) as a benchmark tool. The result is a 20 iteration warm-up and averaged 30 iteration measured. The configuration of `Sketch` uses cmapSize as 20, cmapNo as 2, and an uncompressed counter. You can also benchmark yourself with the [benchmark](../flip-bench/README.md) document.

| function | queueSize = 0 | queueSize = 50 |
| --- | --- |
| `construct` | 711 | 730 |
| `count` | 1,462 | 2,201 |
| `probability` | 2,186 | 3,738 |
| `sampling` | 96,532 | 117,741 |
| `narrowUpdate` | 418 | 847 |
| `deepUpdate` | 478,737 | 354,757 |
|`rearrange` | 361,937 | 1,212,996 |
| `pure` | | |
| `map` | | |
| `flatMap` | 1,564,522 | 1,409,802 |


## Comparison of several density estimation algorithms

To evaluate the performance of *Flip*, I compare it with nonparametric density estimation algorithms such as the conventional histogram and [online kernel density estimation](https://github.com/joluet/okde-java).

The extensible histogram, or xHistogram for short, to be used as the histogram here, is a special case of `Sketch` of *Flip*. Update process of `Sketch` internally divides into two types: `narrowUpdate` and `deepUpdate`. `narrowUpdate` changes count only, not bin, while `deepUpdate` changes both bin and count. After all, the `Sketch` initialized with same width bins and `narrowUpdate`d is essentially the same as a conventional histogram. Of course, its performance is better than the conventional histogram due to the performance enhancements algorithms of `Sketch`.

Online kernel density estimation, or oKDE for short, is an improvement for kernel density estimation to handle data streams with high performance. This algorithm is one of the best performance algorithms among density estimation.

I measure *Flip* in two environments: `rearrange` to adjust bin periodically, and `rearrange` only once when I know that the statistical property of the data stream is stationary. In this comparison, xHistogram initially sets the optimal bin with prior knowledge of the data stream. Therefore, the performance of xHistogram is the upper limit of the performance of *Flip*.

The benchmark environment is the Macbook Pro 2016 with 2.9 GHz Intel Core i5 and 16 GB Memory. The forgetting factor of oKDE is set to 0.9.

| | *Flip* (periodic) | *Flip* (once) | xHistogram | oKDE |
| --- | --- | --- | --- | 
| Execution Time (μs) | 8,363 | 1,390 | 539 | 25,311 | 
| KL-divergence | 0.02 | 0.02 | 0.01 | 0.25 | 
| Euclidean distance | 0.18 | 0.12 | 0.11 |  0.17 | 

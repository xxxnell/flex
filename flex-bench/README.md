# Benchmarks and Experiments

## Benckmark

### Run 

You can run them like:

```
sbt "benchmark"
```

Or, at sbt console, 

```
sbt:flex> benchmark
```

After running the benchmark using jmh, a file with the name `benchmark-$ {current-time} .out` will be created in `flex-bench/benchmarks` directory. This is a benchmark result in csv format with `,` as delimiters.

<!-- In this command, `-i 3` says that we want to run each benchmark with 3 iterations, `-wi 3` says to run 3 warmup iterations, -f 1 says to fork once on each benchmark, and -t1 says to run on one thread. -->

## Experiment

### Run

You can run the experiments like:

```
sbt "experiment"
```

Or, at sbt console, 

```
sbt:flex> experiment
```


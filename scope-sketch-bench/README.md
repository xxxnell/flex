# Scope Sketch Benchmark

You can run them like:

`project scope-sketch-bench jmh:run -i 3 -wi 3 -f 1 -t1`

or 

`project scope-sketch-bench` 
`jmh:run -i 3 -wi 3 -f 1 -t1`

In this command, `-i 3` says that we want to run each benchmark with 3 iterations, `-wi 3` says to run 3 warmup iterations, -f 1 says to fork once on each benchmark, and -t1 says to run on one thread
from os import listdir
from os.path import isfile, join
import re
import numpy as np
from scipy.stats import norm
from scipy.stats import lognorm
import matplotlib as mpl
import matplotlib.pyplot as plt
import preprocessing as prep
import pdfplot as pdfplt
import kldplot as kldplt

name = "update-performance"

fig = plt.figure(figsize=(5, 5))

ax1 = plt.subplot(1, 1, 1)

# Sketch

# sketch_performance_data = prep.data("data/sketch-performance.out", 2)
#
# x_sketch = list(map(lambda d: d[0], sketch_performance_data))
# y_sketch = list(map(lambda d: d[1] , sketch_performance_data))
# ax1.plot(x_sketch, y_sketch)

dir = "../../../flip-bench/benchmarks/"
files = [f for f in listdir(dir) if isfile(join(dir, f))]
last_file_dir = dir + sorted(files)[-1]

sketch_benchmark_data = prep.data_str(last_file_dir)
sketch_performance_data = list(filter(lambda d: d[0] == "flip.benchmark.IterateBench.iterate", sketch_benchmark_data))

regex = re.compile(r"iterateBenchSize -> (\d+)")
sketch_params = list(map(lambda d: d[1], sketch_performance_data))
sketch_count = list(map(lambda params: int(regex.search(params).group(1)), sketch_params))
sketch_score = list(map(lambda d: float(d[2]), sketch_performance_data))

ax1.loglog(sketch_count, sketch_score, label = "Sketch")


# oKDE
okde_loc = "data/okde-performance.out"
okde_performance_data = prep.data(okde_loc, 2)

okde_count = list(map(lambda d: d[0], okde_performance_data))
okde_score = list(map(lambda d: d[1] , okde_performance_data))
ax1.loglog(okde_count, okde_score, label = "oKDE")


# Label

ax1.set_title("Comparison of density estimations")
ax1.set_ylabel("execution time (ns)")
ax1.set_xlabel("update count")
ax1.set_xlim(1E2)
ax1.set_ylim(8E5)


# Save Plot

plt.legend()
if len(sketch_count) != 0:
    print(len(sketch_count) != 0)
    plt.savefig(name + '.pdf')
    plt.savefig(name + '.png')
else:
    print("Benchmark contains no performance records.")

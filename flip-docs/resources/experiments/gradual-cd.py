import numpy as np
from scipy.stats import pareto
import matplotlib as mpl
import matplotlib.pyplot as plt
import preprocessing as prep

name = "gradual-cd-normal"
dir = "../../flip-bench/experiments/gradual-cd-normal/"

fig = plt.figure(figsize=(10, 5))

nrows = 1
ncols = 2
xmin = 10
xmax = 1000

# Median

median_data_loc = dir + "gradual-cd-normal-median.out"

median_data = prep.data(median_data_loc, 3)

x1 = list(map(lambda d: d[0], median_data))
y1 = list(map(lambda d: d[1], median_data))
y2 = list(map(lambda d: d[2], median_data))

sp1 = plt.subplot(nrows, ncols, 1)
sp1.plot(x1, y2)
sp1.plot(x1, y1, '--')
sp1.axvline(300, color='r', linestyle=':', linewidth=1)
sp1.set_ylabel("median")
sp1.set_xlabel("update count")
sp1.set_xlim(xmin, xmax)
# sp1.set_ylim(0.0, 20.0)
sp1.set_ylim(0.0, 8.0)

# KLD

kld_data_loc = dir + "gradual-cd-normal-kld.out"

kld_data = prep.data(kld_data_loc, 2)

x3 = list(map(lambda d: d[0], kld_data))
y3 = list(map(lambda d: abs(d[1]), kld_data))
sp2 = plt.subplot(nrows, ncols, 2)
sp2.plot(x3, y3)
sp2.axvline(300, color='r', linestyle=':', linewidth=1)
sp2.set_ylabel("divergence")
sp2.set_xlabel("update count")
sp2.set_xlim(xmin, xmax)
# sp2.set_ylim(0.0, 1.5)
sp2.set_ylim(0.0, 0.8)

plt.legend()
plt.savefig(name + '.pdf')
plt.savefig(name + '.png')
plt.show()

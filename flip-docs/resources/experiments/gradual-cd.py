import numpy as np
from scipy.stats import norm
import matplotlib as mpl
import matplotlib.pyplot as plt
import preprocessing as prep
import pdfplot as pdfplt
import kldplot as kldplt

name = "gradual-cd-normal"
dir = "../../../flip-bench/experiments/gradual-cd-normal/"

fig = plt.figure(figsize=(10, 5))

nrows = 1
ncols = 2
countmin = 40
countmax = 1000

moving_start = 300
velocity = 0.01

def data_loc(i):
    return dir + "gradual-cd-normal-pdf-" + str(i) + ".out"

def expected(x, start, i):
    if i < start:
        return norm.pdf(x)
    else:
        return norm.pdf(np.array(x) - (i - start) * velocity)


# Median

medianmin = -0.5
medianmax = 8

median_data_loc = dir + "gradual-cd-normal-median.out"

median_data = prep.data(median_data_loc, 3)

x1 = list(map(lambda d: d[0], median_data))
y1 = list(map(lambda d: d[1], median_data))
y2 = list(map(lambda d: d[2], median_data))

ax1 = plt.subplot(nrows, ncols, 1)
ax1.plot(x1, y2)
ax1.plot(x1, y1, '--')
ax1.axvline(moving_start, color='r', linestyle=':', linewidth=1)
ax1.set_ylabel("median")
ax1.set_xlabel("update count")
ax1.set_xlim(countmin, countmax)
ax1.set_ylim(medianmin, medianmax)


# KLD

rearr_start = 50
rearr_period = 100
kld_max = 0.8

kld_data_loc = dir + "gradual-cd-normal-kld.out"

ax2 = plt.subplot(nrows, ncols, 2)
kldplt.kldplot(ax2, kld_data_loc, kld_max, countmin, countmax, rearr_start, rearr_period, False)
ax2.axvline(moving_start, color='r', linestyle=':', linewidth=1)


# Save

# plt.legend()
plt.savefig(name + '.pdf')
plt.savefig(name + '.png')


# Animated PDF

step = 10
fps = 4
xmin = -1
xmax = 8
ymin = 0
ymax = 1

utd_animation2 = pdfplt.animated_pdfplot_bar(
  data_loc, countmin, countmax, step, lambda x, i: expected(x, moving_start, i), xmin, xmax, ymin, ymax)
utd_animation2.save(name + '-histo.gif', writer='imagemagick', fps=fps)

import numpy as np
from scipy.stats import norm
import matplotlib as mpl
import matplotlib.pyplot as plt
import preprocessing as prep
import pdfplot as pdfplt
import cdfplot as cdfplt
import kldplot as kldplt

name = "incremental-cd-normal"
dir = "../../../flip-bench/experiments/incremental-cd-normal/"

fig = plt.figure(figsize=(10, 5))

nrows = 1
ncols = 2
countmin = 40
countmax = 1000

moving_start = 300
velocity = 0.01

def pdf_data_loc(i):
    return dir + "incremental-cd-normal-pdf-" + str(i) + ".out"

def cdf_data_loc(i):
    return dir + "incremental-cd-normal-cdf-" + str(i) + ".out"

def delta_data_loc(i):
    return dir + "incremental-cd-normal-delta-" + str(i) + ".out"

def pdf_expected(x, start, i):
    if i < start:
        return norm.pdf(x)
    else:
        return norm.pdf(np.array(x) - (i - start) * velocity)

def cdf_expected(x, start, i):
    if i < start:
        return norm.cdf(x)
    else:
        return norm.cdf(np.array(x) - (i - start) * velocity)


# Median

medianmin = -0.5
medianmax = 8

median_data_loc = dir + "incremental-cd-normal-median.out"

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

kld_data_loc = dir + "incremental-cd-normal-kld.out"

ax2 = plt.subplot(nrows, ncols, 2)
kldplt.kldplot(ax2, kld_data_loc, kld_max, countmin, countmax, rearr_start, rearr_period, False)
ax2.axvline(moving_start, color='r', linestyle=':', linewidth=1)


# Save

# plt.legend()
plt.savefig(name + '.pdf')
plt.savefig(name + '.png')


# ED

rearr_start = 50
rearr_period = 100
ed_max = 0.25

ed_data_loc = dir + "incremental-cd-normal-ed.out"

fig = plt.figure()
axed = fig.add_subplot(1, 1, 1)
axed.set_ylabel("$D_Δ$")
kldplt.distplot(axed, ed_data_loc, ed_max, countmin, countmax, rearr_start, rearr_period, False)
axed.axvline(moving_start, color='r', linestyle=':', linewidth=1)


# Animated PDF

start = 10
end = 1000
step = 10
fps = 4

xmin = -1
xmax = 8
ymin = 0
ymax = 1


# PDF: Animated

utd_animation2 = pdfplt.animated_pdfplot_bar(
  pdf_data_loc, countmin, countmax, step, lambda x, i: pdf_expected(x, moving_start, i), xmin, xmax, ymin, ymax)
utd_animation2.save(name + '-pdf.gif', writer='imagemagick', fps=fps)


# CDF: Animated

cdf_animation = cdfplt.animated_cdfplot(
  cdf_data_loc, delta_data_loc, start, end, step, lambda x, i: cdf_expected(x, moving_start, i), xmin, xmax, 0, 1)
cdf_animation.save(name + '-cdf.gif', writer='imagemagick', fps=fps)

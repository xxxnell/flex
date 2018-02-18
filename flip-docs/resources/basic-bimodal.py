import numpy as np
from scipy.stats import norm
import matplotlib as mpl
import matplotlib.pyplot as plt
import preprocessing as prep

name = "basic-bimodal"
dir = "../../flip-bench/experiments/basic-bimodal/"

fig = plt.figure(figsize=(20, 5))

# PDF

xmin = -5
xmax = 5
ymax = 0.3

data_locs = [
    dir + "basic-bimodal-pdf-40.out",
    dir + "basic-bimodal-pdf-60.out",
    dir + "basic-bimodal-pdf-340.out"
]

i = 1
for data_loc in data_locs:
    data_xmin = xmin - 10
    data_xmax = xmax + 10

    raw_dat = prep.data(data_loc, 3)
    dat = prep.transform(raw_dat, data_xmin, data_xmax)

    x1, xerr1, y1 = prep.unzip(dat)
    x2 = np.arange(data_xmin, data_xmax, 0.1)
    y2 = 0.5 * norm.pdf(x2 - 2) + 0.5 * norm.pdf(x2 + 2)

    ax = plt.subplot(1, len(data_locs) + 1, i)
    ax.errorbar(x1, y1, xerr=xerr1, fmt='.')
    ax.plot(x2, y2, '--')
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(0, ymax)
    ax.set_ylabel("probability density")
    ax.set_xlabel("x")

    i = i + 1


# KLD

countmin = 40
countmax = 500
rearr_start = 50
rearr_period = 100

kld_data_loc = dir + "basic-bimodal-kld.out"

kld_data = prep.data(kld_data_loc, 2)

x3 = list(map(lambda d: d[0], kld_data))
y3 = list(map(lambda d: abs(d[1]) , kld_data))
ax = plt.subplot(1, len(data_locs) + 1, len(data_locs) + 1)
ax.plot(x3, y3)
for i in range(rearr_start, countmax, rearr_period):
    ax.axvline(i, color='r', linestyle=':', linewidth=1)
ax.set_ylabel("KL divergence")
ax.set_xlabel("update count")
ax.set_xlim(countmin, countmax)
ax.set_ylim(0.0, 0.13)

plt.legend()
plt.savefig(name + '.pdf')
plt.savefig(name + '.png')
plt.show()

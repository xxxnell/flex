import numpy as np
from scipy.stats import pareto
import matplotlib as mpl
import matplotlib.pyplot as plt
import preprocessing as prep
import pdfplot as pdfplt
import kldplot as kldplt

name = "basic-pareto"
dir = "../../../flip-bench/experiments/basic-pareto/"

def data_loc(i):
    return dir + "basic-pareto-pdf-" + str(i) + ".out"

def expected(x):
    return pareto.pdf(x, 1)


# PDF: Represented

xmin = 0
xmax = 10
ymin = 0
ymax = 1

fig = plt.figure(figsize=(20, 5))

data_locs = [
    data_loc(40),
    data_loc(60),
    data_loc(220)
]

i = 1
for _data_loc in data_locs:
    ax = plt.subplot(1, len(data_locs) + 1, i)
    # pdfplt.pdfplot_errorbar(ax, expected, _data_loc, xmin, xmax, ymin, ymax)
    pdfplt.pdfplot_bar(ax, expected, _data_loc, xmin, xmax, ymin, ymax)
    i = i + 1


# KLD

countmin = 40
countmax = 500
rearr_start = 50
rearr_period = 100
kld_max = 0.15

kld_data_loc = dir + "basic-pareto-kld.out"

ax = plt.subplot(1, len(data_locs) + 1, len(data_locs) + 1)
kldplt.kldplot(ax, kld_data_loc, kld_max, countmin, countmax, rearr_start, rearr_period)


# Save Plot

plt.legend()
plt.savefig(name + '.pdf')
plt.savefig(name + '.png')
plt.show()


# PDF: Animated

start = 10
end = 500
step = 10
fps = 4

utd_animation1 = pdfplt.animated_pdfplot_errorbar(data_loc, start, end, step, expected, xmin, xmax, ymin, ymax)
utd_animation1.save(name + '-errorbar.gif', writer='imagemagick', fps=fps)

utd_animation2 = pdfplt.animated_pdfplot_bar(data_loc, start, end, step, expected, xmin, xmax, ymin, ymax)
utd_animation2.save(name + '-histo.gif', writer='imagemagick', fps=fps)

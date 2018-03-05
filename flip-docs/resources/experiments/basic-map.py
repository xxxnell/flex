import numpy as np
from scipy.stats import norm
from scipy.stats import lognorm
import matplotlib as mpl
import matplotlib.pyplot as plt
import preprocessing as prep
import pdfplot as pdfplt
import kldplot as kldplt

name = "basic-map"
dir = "../../../flip-bench/experiments/basic-map/"

fig = plt.figure(figsize=(10, 5))


def prev_expected(x, i = -1):
    return norm.pdf(x)

def post_expected(x, i = -1):
    return lognorm.pdf(x, 1)


# PDF

prev_xmin = -3
prev_xmax = 3
prev_ymin = 0
prev_ymax = 0.6

post_xmin = -1
post_xmax = 7
post_ymin = 0
post_ymax = 0.8

prev_data_loc = dir + "basic-map-prev-pdf.out"
post_data_loc = dir + "basic-map-post-pdf.out"

ax = plt.subplot(1, 2, 1)
pdfplt.pdfplot_bar(ax, prev_expected, prev_data_loc, prev_xmin, prev_xmax, prev_ymin, prev_ymax)
ax.set_title("PDF before map")
ax = plt.subplot(1, 2, 2)
pdfplt.pdfplot_bar(ax, post_expected, post_data_loc, post_xmin, post_xmax, post_ymin, post_ymax)
ax.set_title("PDF after map")


# Save Plot

plt.legend()
plt.savefig(name + '.pdf')
plt.savefig(name + '.png')
plt.show()

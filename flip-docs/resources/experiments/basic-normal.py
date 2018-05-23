import numpy as np
from scipy.stats import norm
import matplotlib as mpl
import matplotlib.pyplot as plt
import preprocessing as prep
import pdfplot as pdfplt
import cdfplot as cdfplt
import kldplot as kldplt

name = "basic-normal"
dir = "../../../flip-bench/experiments/basic-normal/"

fig = plt.figure(figsize=(20, 5))

def pdf_data_loc(i):
    return dir + "basic-normal-pdf-" + str(i) + ".out"

def pdf_expected(x, i = -1):
    return norm.pdf(x)

def cdf_data_loc(i):
    return dir + "basic-normal-cdf-" + str(i) + ".out"

def cdf_expected(x, i = -1):
    return norm.cdf(x)


# PDF: Represented

xmin = -3
xmax = 3
ymin = 0
ymax = 0.6

data_counts = [40, 60, 340]

i = 0
for data_count in data_counts:
    ax = plt.subplot(1, len(data_counts) + 1, i + 1)
    annotation = "(" + chr(ord('a') + i) + ")" + " pdf at update count: " + str(data_count)
    ax.set_title(annotation)
    pdfplt.pdfplot_bar(ax, pdf_expected, pdf_data_loc(data_count), xmin, xmax, ymin, ymax)
    i = i + 1


# KLD

countmin = 40
countmax = 500
rearr_start = 50
rearr_period = 100
kld_max = 0.15

kld_pdf_data_loc = dir + "basic-normal-kld.out"

ax = plt.subplot(1, len(data_counts) + 1, len(data_counts) + 1)
ax.set_title("(d) KL-divergence")
kldplt.kldplot(ax, kld_pdf_data_loc, kld_max, countmin, countmax, rearr_start, rearr_period)


# Save Plot

# plt.legend()
plt.savefig(name + '.pdf')
plt.savefig(name + '.png')


# Animated

start = 10
end = 500
step = 10
fps = 4

# PDF: Animated

pdf_animation = pdfplt.animated_pdfplot_bar(pdf_data_loc, start, end, step, pdf_expected, xmin, xmax, ymin, ymax)
pdf_animation.save(name + '-pdf.gif', writer='imagemagick', fps=fps)


# CDF: Animated

cdf_animation = cdfplt.animated_cdfplot(cdf_data_loc, start, end, step, cdf_expected, xmin, xmax, 0, 1)
cdf_animation.save(name + '-cdf.gif', writer='imagemagick', fps=fps)

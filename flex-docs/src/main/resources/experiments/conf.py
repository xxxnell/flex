from os import listdir
from os.path import isfile, join
import re
import numpy as np
from scipy.stats import norm
from scipy.stats import lognorm
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.mlab import griddata
import preprocessing as prep
import pdfplot as pdfplt
import kldplot as kldplt
from matplotlib import rcParams

name = "conf"
bench_dir = "data/sketch-benchmark.out"
exp_dir = "data/"
normal_out = "conf-normal.out"
incr_drift_out = "conf-incr-drift.out"
rcParams['font.family'] = 'serif'
fontsize = 35
rcParams.update({'font.size': fontsize})

"""
Bench
   cmapSizeLS -> 20 & decayFactorLS -> 1.0 & rebuildThresholdLS -> 0.1 & iterateBenchSizeLS -> 1000 & cmapNoLS -> 10 & bufferSizeLS -> 50

files = [f for f in listdir(bench_dir) if isfile(join(bench_dir, f))]
last_file_dir = bench_dir + sorted(files)[-1]
"""

bench_raw = prep.data_str(bench_dir)
normal_raw = list(filter(lambda d: d[0] == "flex.benchmark.ConfBench.normal", bench_raw))
incr_drift_raw = list(filter(lambda d: d[0] == "flex.benchmark.ConfBench.incrDrift", bench_raw))

norgx = "([-+]?[0-9]*\.?[0-9]*)"

def bench_data(init, raw):
    for d in raw:
        cmapsize = re.compile(r"cmapSizeLS -> " + norgx).search(d[1]).group(1)
        cmapno = re.compile(r"cmapNoLS -> " + norgx).search(d[1]).group(1)
        buffersize = re.compile(r"bufferSizeLS -> " + norgx).search(d[1]).group(1)
        decayfactor = re.compile(r"decayFactorLS -> " + norgx).search(d[1]).group(1)
        rebuildth = re.compile(r"rebuildThresholdLS -> " + norgx).search(d[1]).group(1)
        score = float(re.compile(r"iterateBenchSizeLS -> " + norgx).search(d[1]).group(1)) * float(d[2]) / 1000000
        init[repr([cmapsize, cmapno, buffersize, decayfactor, rebuildth])] = score
    return init

normal_bench = bench_data({}, normal_raw)
incr_drift_bench = bench_data({}, incr_drift_raw)


"""
Exp
  conf.cmap.size, conf.cmap.no, conf.bufferSize, conf.decayFactor, conf.rebuildThreshold
"""

normal_exp_raw = prep.data_str(exp_dir + normal_out)
incr_drift_exp_raw = prep.data_str(exp_dir + incr_drift_out)

def dist_data(init, raw):
    for d in raw:
        try:
            init[repr([d[0], d[1], d[2], d[3], d[4]])] = float(d[5])
        except ValueError:
            pass
    return init

def mem_data(init, raw):
    for d in raw:
        try:
            init[repr([d[0], d[1], d[2], d[3], d[4]])] = float(d[6]) / 1000
        except ValueError:
            pass
    return init

normal_dist = dist_data({}, normal_exp_raw)
normal_mem = mem_data({}, normal_exp_raw)
incr_drift_dist = dist_data({}, incr_drift_exp_raw)
incr_drift_mem = mem_data({}, incr_drift_exp_raw)

"""
Join
"""

def join(bench_data, dist_data, mem_data):
    x = []
    y = []
    z = []
    for params in bench_data:
        score = bench_data[params]
        dist = dist_data[params]
        mem = mem_data[params]
        if score and dist and mem:
            x.append(dist)
            y.append(score)
            z.append(mem)
    return [x, y, z]

normal_data = join(normal_bench, normal_dist, normal_mem)
incr_drift_data = join(incr_drift_bench, incr_drift_dist, incr_drift_mem)


def parr(params):
    # cmapSize, cmapNo, bufferSize, decayFactor, rebuildThreshold
    search = re.compile(r"(\['" + norgx + "', '" + norgx + "', '" + norgx + "', '" + norgx + "', '" + norgx + "'\])").search(params)
    return [int(search.group(2)), int(search.group(3)), int(search.group(4)), float(search.group(5)), float(search.group(6))]

def cmapsize_filter(data0, cmapno, buffersize, decayfactor, rebuildth):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[1], parr(p)[2], parr(p)[3], parr(p)[4]] == [cmapno, buffersize, decayfactor, rebuildth] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[0]) }
    return data2

def cmapsize_normal_join(cmapno, buffersize, decayfactor, rebuildth):
    return join(cmapsize_filter(normal_bench, cmapno, buffersize, decayfactor, rebuildth), normal_dist, normal_mem)

def cmapsize_incr_drift_join(cmapno, buffersize, decayfactor, rebuildth):
    return join(cmapsize_filter(incr_drift_bench, cmapno, buffersize, decayfactor, rebuildth), incr_drift_dist, incr_drift_mem)

normal_data_cmapsize = [
    cmapsize_normal_join(2, 10, 2.5, 0.01),
    cmapsize_normal_join(2, 10, 2.5, 0.3),
    cmapsize_normal_join(2, 10, 2.5, 0.7),
    cmapsize_normal_join(2, 70, 1.0, 0.3),
    cmapsize_normal_join(2, 70, 1.0, 0.7),
    cmapsize_normal_join(5, 70, 1.0, 0.7),
    cmapsize_normal_join(2, 70, 1.0, 0.01),
    cmapsize_normal_join(2, 70, 2.5, 0.7),
    cmapsize_normal_join(2, 150, 0.2, 0.3),
    cmapsize_normal_join(2, 150, 2.5, 0.3),
    cmapsize_normal_join(2, 150, 1.0, 0.3),
    cmapsize_normal_join(2, 150, 1.0, 0.3),
]
inc_drift_data_cmapsize = [
    cmapsize_incr_drift_join(2, 10, 2.5, 0.01),
    cmapsize_incr_drift_join(2, 10, 2.5, 0.3),
    cmapsize_incr_drift_join(2, 10, 2.5, 0.7),
    cmapsize_incr_drift_join(2, 70, 1.0, 0.3),
    # cmapsize_incr_drift_join(2, 70, 1.0, 0.7),
    # cmapsize_incr_drift_join(5, 70, 1.0, 0.7),
    cmapsize_incr_drift_join(2, 70, 1.0, 0.01),
    # cmapsize_incr_drift_join(2, 70, 2.5, 0.7),
    cmapsize_incr_drift_join(2, 150, 0.2, 0.3),
    cmapsize_incr_drift_join(2, 150, 2.5, 0.3),
    cmapsize_incr_drift_join(2, 150, 1.0, 0.3),
    cmapsize_incr_drift_join(2, 150, 1.0, 0.3),
]

def cmapno_filter(data0, cmapsize, buffersize, decayfactor, rebuildth):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[0], parr(p)[2], parr(p)[3], parr(p)[4]] == [cmapsize, buffersize, decayfactor, rebuildth] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[1]) }
    return data2

def cmapno_normal_join(cmapsize, buffersize, decayfactor, rebuildth):
    return join(cmapno_filter(normal_bench, cmapsize, buffersize, decayfactor, rebuildth), normal_dist, normal_mem)

def cmapno_incr_drift_join(cmapsize, buffersize, decayfactor, rebuildth):
    return join(cmapno_filter(incr_drift_bench, cmapsize, buffersize, decayfactor, rebuildth), incr_drift_dist, incr_drift_mem)

normal_data_cmapno = [
    # cmapno_normal_join(10, 30, 1.0, 0.09),
    # cmapno_normal_join(10, 70, 1.0, 0.09),
    cmapno_normal_join(10, 120, 1.0, 0.09),
    cmapno_normal_join(40, 30, 1.0, 0.09),
    cmapno_normal_join(40, 30, 1.0, 0.3),
    cmapno_normal_join(40, 70, 1.0, 0.3),
    cmapno_normal_join(40, 70, 1.0, 0.7),
    cmapno_normal_join(100, 70, 1.0, 0.09),
    cmapno_normal_join(100, 70, 1.0, 0.3),
    cmapno_normal_join(100, 70, 1.0, 0.3),
    # cmapno_normal_join(100, 10, 2.0, 0.7),
    cmapno_normal_join(100, 30, 2.0, 0.7),
    # cmapno_normal_join(100, 70, 2.0, 0.7),
    # cmapno_normal_join(100, 120, 2.0, 0.7)
]
inc_drift_data_cmapno = [
    # cmapno_incr_drift_join(10, 30, 1.0, 0.09),
    # cmapno_incr_drift_join(10, 70, 1.0, 0.09),
    cmapno_incr_drift_join(10, 120, 1.0, 0.09),
    cmapno_incr_drift_join(40, 30, 1.0, 0.09),
    cmapno_incr_drift_join(40, 30, 1.0, 0.3),
    cmapno_incr_drift_join(40, 70, 1.0, 0.3),
    cmapno_incr_drift_join(40, 70, 1.0, 0.7),
    cmapno_incr_drift_join(100, 70, 1.0, 0.09),
    cmapno_incr_drift_join(100, 70, 1.0, 0.3),
    cmapno_incr_drift_join(100, 70, 1.0, 0.3),
    cmapno_incr_drift_join(100, 10, 2.0, 0.7),
    # cmapno_incr_drift_join(100, 30, 2.0, 0.7),
    # cmapno_incr_drift_join(100, 70, 2.0, 0.7),
    # cmapno_incr_drift_join(100, 120, 2.0, 0.7)
]


def buffersize_filter(data0, cmapsize, cmapno, decayfactor, rebuildth):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[0], parr(p)[1], parr(p)[3], parr(p)[4]] == [cmapsize, cmapno, decayfactor, rebuildth] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[2]) }
    return data2

def buffersize_normal_join(cmapsize, cmapno, decayfactor, rebuildth):
    return join(buffersize_filter(normal_bench, cmapsize, cmapno, decayfactor, rebuildth), normal_dist, normal_mem)

def buffersize_incr_drift_join(cmapsize, cmapno, decayfactor, rebuildth):
    return join(buffersize_filter(incr_drift_bench, cmapsize, cmapno, decayfactor, rebuildth), incr_drift_dist, incr_drift_mem)

normal_data_buffersize = [
    buffersize_normal_join(10, 2, 1.0, 0.09),
    buffersize_normal_join(10, 3, 1.0, 0.3),
    buffersize_normal_join(10, 3, 2.0, 0.3),
    buffersize_normal_join(10, 3, 2.0, 0.7),
    buffersize_normal_join(25, 10, 0.2, 0.01),
    buffersize_normal_join(40, 2, 1.0, 0.09),
    buffersize_normal_join(40, 3, 1.0, 0.3),
    buffersize_normal_join(40, 3, 2.0, 0.3),
    buffersize_normal_join(40, 3, 2.0, 0.7),
    buffersize_normal_join(100, 2, 1.0, 0.09),
    # buffersize_normal_join(100, 3, 1.0, 0.3),
    # buffersize_normal_join(100, 3, 2.0, 0.3),
    # buffersize_normal_join(100, 3, 2.0, 0.7)
]
inc_drift_data_buffersize = [
    # buffersize_incr_drift_join(10, 2, 1.0, 0.09),
    # buffersize_incr_drift_join(10, 3, 1.0, 0.3),
    # buffersize_incr_drift_join(10, 3, 2.0, 0.3),
    buffersize_incr_drift_join(10, 3, 2.0, 0.7),
    buffersize_incr_drift_join(25, 10, 0.2, 0.01),
    buffersize_incr_drift_join(40, 2, 1.0, 0.09),
    buffersize_incr_drift_join(40, 3, 1.0, 0.3),
    buffersize_incr_drift_join(40, 3, 2.0, 0.3),
    buffersize_incr_drift_join(40, 3, 2.0, 0.7),
    buffersize_incr_drift_join(100, 2, 1.0, 0.09),
    # buffersize_incr_drift_join(100, 3, 1.0, 0.3),
    # buffersize_incr_drift_join(100, 3, 2.0, 0.3),
    # buffersize_incr_drift_join(100, 3, 2.0, 0.7)
]

def decayfactor_filter(data0, cmapsize, cmapno, buffersize, rebuildth):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[0], parr(p)[1], parr(p)[2], parr(p)[4]] == [cmapsize, cmapno, buffersize, rebuildth] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[3]) }
    return data2

def decayfactor_normal_join(cmapsize, cmapno, buffersize, rebuildth):
    return join(decayfactor_filter(normal_bench, cmapsize, cmapno, buffersize, rebuildth), normal_dist, normal_mem)

def decayfactor_incr_drift_join(cmapsize, cmapno, buffersize, rebuildth):
    return join(decayfactor_filter(incr_drift_bench, cmapsize, cmapno, buffersize, rebuildth), incr_drift_dist, incr_drift_mem)

normal_data_decayfactor = [
    decayfactor_normal_join(10, 2, 30, 0.09),
    decayfactor_normal_join(10, 2, 30, 0.3),
    decayfactor_normal_join(10, 2, 70, 0.3),
    decayfactor_normal_join(10, 2, 70, 0.7),
    decayfactor_normal_join(40, 2, 30, 0.09),
    decayfactor_normal_join(40, 2, 30, 0.3),
    decayfactor_normal_join(40, 2, 70, 0.3),
    decayfactor_normal_join(40, 2, 70, 0.7),
    decayfactor_normal_join(100, 2, 30, 0.09),
    decayfactor_normal_join(100, 2, 30, 0.3),
    decayfactor_normal_join(100, 2, 70, 0.3),
    decayfactor_normal_join(100, 2, 70, 0.7)
]
inc_drift_data_decayfactor = [
    decayfactor_incr_drift_join(10, 2, 30, 0.09),
    decayfactor_incr_drift_join(10, 2, 30, 0.3),
    decayfactor_incr_drift_join(10, 2, 70, 0.3),
    decayfactor_incr_drift_join(10, 2, 70, 0.7),
    decayfactor_incr_drift_join(40, 2, 30, 0.09),
    decayfactor_incr_drift_join(40, 2, 30, 0.3),
    decayfactor_incr_drift_join(40, 2, 70, 0.3),
    decayfactor_incr_drift_join(40, 2, 70, 0.7),
    decayfactor_incr_drift_join(100, 2, 30, 0.09),
    decayfactor_incr_drift_join(100, 2, 30, 0.3),
    decayfactor_incr_drift_join(100, 2, 70, 0.3),
    decayfactor_incr_drift_join(100, 2, 70, 0.7)
]

def rebuildth_filter(data0, cmapsize, cmapno, buffersize, decayfactor):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[0], parr(p)[1], parr(p)[2], parr(p)[3]] == [cmapsize, cmapno, buffersize, decayfactor] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[4]) }
    return data2

def rebuildth_normal_join(cmapsize, cmapno, buffersize, decayfactor):
    return join(rebuildth_filter(normal_bench, cmapsize, cmapno, buffersize, decayfactor), normal_dist, normal_mem)

def rebuildth_incr_drift_join(cmapsize, cmapno, buffersize, decayfactor):
    return join(rebuildth_filter(incr_drift_bench, cmapsize, cmapno, buffersize, decayfactor), incr_drift_dist, incr_drift_mem)


normal_data_rebuildth = [
    # rebuildth_normal_join(10, 2, 30, 1.0),
    # rebuildth_normal_join(10, 2, 70, 2.0),
    # rebuildth_normal_join(40, 3, 30, 1.0),
    # rebuildth_normal_join(40, 3, 70, 2.0),
    rebuildth_normal_join(40, 2, 30, 1.0),
    rebuildth_normal_join(100, 2, 70, 2.0),
    rebuildth_normal_join(100, 3, 30, 1.0),
    rebuildth_normal_join(100, 3, 70, 2.0),
    rebuildth_normal_join(100, 2, 30, 1.0)
]
inc_drift_data_rebuildth = [
    rebuildth_incr_drift_join(10, 2, 30, 1.0),
    rebuildth_incr_drift_join(10, 2, 70, 2.0),
    # rebuildth_incr_drift_join(40, 3, 30, 1.0),
    rebuildth_incr_drift_join(40, 3, 70, 2.0),
    # rebuildth_incr_drift_join(40, 2, 30, 1.0),
    # rebuildth_incr_drift_join(100, 2, 70, 2.0),
    rebuildth_incr_drift_join(100, 3, 30, 1.0),
    rebuildth_incr_drift_join(100, 3, 70, 2.0),
    # rebuildth_incr_drift_join(100, 2, 30, 1.0)
]

"""
Performance Diagram
"""

prefix = 'conf/'

def performance_diag(dist, thro, mem, data=False, figsize=(10,10), alpha=0.8):
    xlim = (0, 0.6)
    ylim = (0, 1.5)
    f, ax = plt.subplots(1, 1, figsize=figsize)

    xi = np.linspace(xlim[0], xlim[1], 100)
    yi = np.linspace(ylim[0], ylim[1], 200)
    zi = griddata(dist, thro, mem, xi, yi, interp='linear')
    intv = [0,4,6,8,10,20]
    if data:
        plt.contour(xi, yi, zi, intv, linewidths=0.5, colors='k')
    plt.contourf(xi, yi, zi, intv, cmap=plt.cm.viridis, alpha=alpha)

    cb = plt.colorbar()
    cb.set_label("Memory (KB)")
    plt.xlim(xlim)
    plt.ylim(ylim)
    plt.ylabel("Throughput (Mops)")
    plt.xlabel("Error ($\delta$)")
    ax.tick_params(axis='both', labelsize=fontsize * 0.85)
    return f, ax

"""
stationary
"""
f, ax = performance_diag(normal_data[0], normal_data[1], normal_data[2], alpha=1.0)

bbox_props = dict(boxstyle="larrow,pad=0.3", fc="w", lw=2)
t = ax.text(0.4, 0.5, "Better", ha="center", va="center", rotation=-45,
            fontsize=fontsize * 1.2,
            bbox=bbox_props)

normal_conf = 'normal-conf'
f.savefig(prefix + normal_conf + '.pdf')
f.savefig(prefix + normal_conf + '.png')

"""
non-stationary
"""
f, ax = performance_diag(incr_drift_data[0], incr_drift_data[1], incr_drift_data[2], alpha=1.0)

incr_drift_conf = 'incr-drift-conf'
f.savefig(prefix + incr_drift_conf + '.pdf')
f.savefig(prefix + incr_drift_conf + '.png')

"""
Performance Trend
"""
talpha=0.3

def arrowplot(ax, trends):
    color = 'r'
    line_width = 2.0
    arrow_head_size = 0.03
    for trend in trends:
        dists = trend[0]
        thros = trend[1]
        ax.plot(dists, thros, color='r', linewidth=line_width)
        arrow = ax.arrow(dists[-2], thros[-2], dists[-1] - dists[-2], thros[-1] - thros[-2],
        color='r', head_width=arrow_head_size)
    return ax

"""
# stationary & cmapsize
"""
f, ax = performance_diag(normal_data[0], normal_data[1], normal_data[2], alpha=talpha)
arrowplot(ax, normal_data_cmapsize)

normal_cmapsize_name = 'normal-cmapsize'
f.savefig(prefix + normal_cmapsize_name + '.pdf')
f.savefig(prefix + normal_cmapsize_name + '.png')

"""
# stationary & cmapno
"""
f, ax = performance_diag(normal_data[0], normal_data[1], normal_data[2], alpha=talpha)
arrowplot(ax, normal_data_cmapno)

normal_cmapno_name = 'normal-cmapno'
plt.savefig(prefix + normal_cmapno_name + '.pdf')
plt.savefig(prefix + normal_cmapno_name + '.png')

"""
# stationary & buffersize
"""
f, ax = performance_diag(normal_data[0], normal_data[1], normal_data[2], alpha=talpha)
arrowplot(ax, normal_data_buffersize)

normal_buffersize_name = 'normal-buffersize'
plt.savefig(prefix + normal_buffersize_name + '.pdf')
plt.savefig(prefix + normal_buffersize_name + '.png')

"""
# stationary & decayfactor
"""
f, ax = performance_diag(normal_data[0], normal_data[1], normal_data[2], alpha=talpha)
arrowplot(ax, normal_data_decayfactor)

normal_decayfactor_name = 'normal-decayfactor'
plt.savefig(prefix + normal_decayfactor_name + '.pdf')
plt.savefig(prefix + normal_decayfactor_name + '.png')

"""
# stationary & rebuildth
"""
f, ax = performance_diag(normal_data[0], normal_data[1], normal_data[2], alpha=talpha)
arrowplot(ax, normal_data_rebuildth)

normal_rebuildth_name = 'normal-rebuildth'
plt.savefig(prefix + normal_rebuildth_name + '.pdf')
plt.savefig(prefix + normal_rebuildth_name + '.png')

"""
# non-stationary & cmapsize
"""
f, ax = performance_diag(incr_drift_data[0], incr_drift_data[1], incr_drift_data[2], alpha=talpha)
arrowplot(ax, inc_drift_data_cmapsize)

incr_drift_cmapsize_name = 'incr-drift-cmapsize'
plt.savefig(prefix + incr_drift_cmapsize_name + '.pdf')
plt.savefig(prefix + incr_drift_cmapsize_name + '.png')

"""
# non-stationary & cmapno
"""
f, ax = performance_diag(incr_drift_data[0], incr_drift_data[1], incr_drift_data[2], alpha=talpha)
arrowplot(ax, inc_drift_data_cmapno)

incr_drift_cmapno_name = 'incr-drift-cmapno'
plt.savefig(prefix + incr_drift_cmapno_name + '.pdf')
plt.savefig(prefix + incr_drift_cmapno_name + '.png')

"""
# non-stationary & buffersize
"""
f, ax = performance_diag(incr_drift_data[0], incr_drift_data[1], incr_drift_data[2], alpha=talpha)
arrowplot(ax, inc_drift_data_buffersize)

incr_drift_buffersize_name = 'incr-drift-buffersize'
plt.savefig(prefix + incr_drift_buffersize_name + '.pdf')
plt.savefig(prefix + incr_drift_buffersize_name + '.png')

"""
# non-stationary & decayfactor
"""
f, ax = performance_diag(incr_drift_data[0], incr_drift_data[1], incr_drift_data[2], alpha=talpha)
arrowplot(ax, inc_drift_data_decayfactor)

incr_drift_decayfactor_name = 'incr-drift-decayfactor'
plt.savefig(prefix + incr_drift_decayfactor_name + '.pdf')
plt.savefig(prefix + incr_drift_decayfactor_name + '.png')

"""
# non-stationary & rebuildth
"""
f, ax = performance_diag(incr_drift_data[0], incr_drift_data[1], incr_drift_data[2], alpha=talpha)
arrowplot(ax, inc_drift_data_rebuildth)

incr_drift_rebuildth_name = 'incr-drift-rebuildth'
plt.savefig(prefix + incr_drift_rebuildth_name + '.pdf')
plt.savefig(prefix + incr_drift_rebuildth_name + '.png')

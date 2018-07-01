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

name = "conf"
bench_dir = "../../../flip-bench/benchmarks/"
exp_dir = "../../../flip-bench/experiments/conf/"
normal_out = "conf-normal.out"
incr_drift_out = "conf-incr-drift.out"


# Bench
#   cmapSizeLS -> 20 & decayFactorLS -> 1.0 & rebuildThresholdLS -> 0.1 & iterateBenchSizeLS -> 1000 & cmapNoLS -> 10 & bufferSizeLS -> 50

files = [f for f in listdir(bench_dir) if isfile(join(bench_dir, f))]
last_file_dir = bench_dir + sorted(files)[-1]

bench_raw = prep.data_str(last_file_dir)
normal_raw = list(filter(lambda d: d[0] == "flip.benchmark.ConfBench.normal", bench_raw))
incr_drift_raw = list(filter(lambda d: d[0] == "flip.benchmark.ConfBench.incrDrift", bench_raw))

norgx = "([-+]?[0-9]*\.?[0-9]*)"

def bench_data(init, raw):
    for d in raw:
        cmapsize = re.compile(r"cmapSizeLS -> " + norgx).search(d[1]).group(1)
        cmapno = re.compile(r"cmapNoLS -> " + norgx).search(d[1]).group(1)
        buffersize = re.compile(r"bufferSizeLS -> " + norgx).search(d[1]).group(1)
        decayfactor = re.compile(r"decayFactorLS -> " + norgx).search(d[1]).group(1)
        rebuildth = re.compile(r"rebuildThresholdLS -> " + norgx).search(d[1]).group(1)
        score = float(re.compile(r"iterateBenchSizeLS -> " + norgx).search(d[1]).group(1)) * float(d[2])
        init[repr([cmapsize, cmapno, buffersize, decayfactor, rebuildth])] = score
    return init

normal_bench = bench_data({}, normal_raw)
incr_drift_bench = bench_data({}, incr_drift_raw)


# Exp
#   conf.cmap.size, conf.cmap.no, conf.bufferSize, conf.decayFactor, conf.rebuildThreshold

normal_exp_raw = prep.data_str(exp_dir + normal_out)
incr_drift_exp_raw = prep.data_str(exp_dir + incr_drift_out)

def exp_data(init, raw):
    for d in raw:
        try:
            init[repr([d[0], d[1], d[2], d[3], d[4]])] = float(d[5])
        except ValueError:
            pass
    return init

normal_exp = exp_data({}, normal_exp_raw)
incr_drift_exp = exp_data({}, incr_drift_exp_raw)


# Join

def join(bench_data, exp_data):
    x = []
    y = []
    for params in bench_data:
        score = bench_data[params]
        error = exp_data[params]
        if score and error :
            x.append(error)
            y.append(score)
    return [x, y]

normal_data = join(normal_bench, normal_exp)
incr_drift_data = join(incr_drift_bench, incr_drift_exp)


def parr(params):
    # cmapSize, cmapNo, bufferSize, decayFactor, rebuildThreshold
    search = re.compile(r"(\['" + norgx + "', '" + norgx + "', '" + norgx + "', '" + norgx + "', '" + norgx + "'\])").search(params)
    return [int(search.group(2)), int(search.group(3)), int(search.group(4)), float(search.group(5)), float(search.group(6))]

def cmapsize_filter(data0, cmapno, buffersize, decayfactor, rebuildth):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[1], parr(p)[2], parr(p)[3], parr(p)[4]] == [cmapno, buffersize, decayfactor, rebuildth] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[0]) }
    return data2

normal_data_cmapsize = [
    join(cmapsize_filter(normal_bench, 2, 10, 2.5, 0.01), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 10, 2.5, 0.3), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 10, 2.5, 0.7), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 70, 1.0, 0.3), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 70, 1.0, 0.7), normal_exp),
    join(cmapsize_filter(normal_bench, 5, 70, 1.0, 0.7), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 70, 1.0, 0.01), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 70, 2.5, 0.7), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 150, 0.2, 0.3), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 150, 2.5, 0.3), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 150, 1.0, 0.3), normal_exp),
    join(cmapsize_filter(normal_bench, 2, 150, 1.0, 0.3), normal_exp)
]
inc_drift_data_cmapsize = [
    join(cmapsize_filter(incr_drift_bench, 2, 10, 2.5, 0.01), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 10, 2.5, 0.3), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 10, 2.5, 0.7), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 70, 1.0, 0.3), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 70, 1.0, 0.7), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 5, 70, 1.0, 0.7), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 70, 1.0, 0.01), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 70, 2.5, 0.7), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 150, 0.2, 0.3), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 150, 2.5, 0.3), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 150, 1.0, 0.3), incr_drift_exp),
    join(cmapsize_filter(incr_drift_bench, 2, 150, 1.0, 0.3), incr_drift_exp)
]

def cmapno_filter(data0, cmapsize, buffersize, decayfactor, rebuildth):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[0], parr(p)[2], parr(p)[3], parr(p)[4]] == [cmapsize, buffersize, decayfactor, rebuildth] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[1]) }
    return data2

normal_data_cmapno = [
    join(cmapno_filter(normal_bench, 10, 30, 1.0, 0.09), normal_exp),
    join(cmapno_filter(normal_bench, 10, 70, 1.0, 0.09), normal_exp),
    join(cmapno_filter(normal_bench, 10, 120, 1.0, 0.09), normal_exp),
    join(cmapno_filter(normal_bench, 40, 30, 1.0, 0.09), normal_exp),
    join(cmapno_filter(normal_bench, 40, 30, 1.0, 0.3), normal_exp),
    join(cmapno_filter(normal_bench, 40, 70, 1.0, 0.3), normal_exp),
    join(cmapno_filter(normal_bench, 40, 70, 1.0, 0.7), normal_exp),
    join(cmapno_filter(normal_bench, 100, 70, 1.0, 0.09), normal_exp),
    join(cmapno_filter(normal_bench, 100, 70, 1.0, 0.3), normal_exp),
    join(cmapno_filter(normal_bench, 100, 70, 1.0, 0.3), normal_exp),
    join(cmapno_filter(normal_bench, 100, 10, 2.0, 0.7), normal_exp),
    join(cmapno_filter(normal_bench, 100, 30, 2.0, 0.7), normal_exp),
    join(cmapno_filter(normal_bench, 100, 70, 2.0, 0.7), normal_exp),
    join(cmapno_filter(normal_bench, 100, 120, 2.0, 0.7), normal_exp)
]
inc_drift_data_cmapno = [
    join(cmapno_filter(incr_drift_bench, 10, 30, 1.0, 0.09), normal_exp),
    join(cmapno_filter(incr_drift_bench, 10, 70, 1.0, 0.09), normal_exp),
    join(cmapno_filter(incr_drift_bench, 10, 120, 1.0, 0.09), normal_exp),
    join(cmapno_filter(incr_drift_bench, 40, 30, 1.0, 0.09), normal_exp),
    join(cmapno_filter(incr_drift_bench, 40, 30, 1.0, 0.3), normal_exp),
    join(cmapno_filter(incr_drift_bench, 40, 70, 1.0, 0.3), normal_exp),
    join(cmapno_filter(incr_drift_bench, 40, 70, 1.0, 0.7), normal_exp),
    join(cmapno_filter(incr_drift_bench, 100, 70, 1.0, 0.09), normal_exp),
    join(cmapno_filter(incr_drift_bench, 100, 70, 1.0, 0.3), normal_exp),
    join(cmapno_filter(incr_drift_bench, 100, 70, 1.0, 0.3), normal_exp),
    join(cmapno_filter(incr_drift_bench, 100, 10, 2.0, 0.7), normal_exp),
    join(cmapno_filter(incr_drift_bench, 100, 30, 2.0, 0.7), normal_exp),
    join(cmapno_filter(incr_drift_bench, 100, 70, 2.0, 0.7), normal_exp),
    join(cmapno_filter(incr_drift_bench, 100, 120, 2.0, 0.7), normal_exp)
]


def buffersize_filter(data0, cmapsize, cmapno, decayfactor, rebuildth):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[0], parr(p)[1], parr(p)[3], parr(p)[4]] == [cmapsize, cmapno, decayfactor, rebuildth] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[2]) }
    return data2

normal_data_buffersize = [
    join(buffersize_filter(normal_bench, 10, 2, 1.0, 0.09), normal_exp),
    join(buffersize_filter(normal_bench, 10, 3, 1.0, 0.3), normal_exp),
    join(buffersize_filter(normal_bench, 10, 3, 2.0, 0.3), normal_exp),
    join(buffersize_filter(normal_bench, 10, 3, 2.0, 0.7), normal_exp),
    join(buffersize_filter(normal_bench, 25, 10, 0.2, 0.01), normal_exp),
    join(buffersize_filter(normal_bench, 40, 2, 1.0, 0.09), normal_exp),
    join(buffersize_filter(normal_bench, 40, 3, 1.0, 0.3), normal_exp),
    join(buffersize_filter(normal_bench, 40, 3, 2.0, 0.3), normal_exp),
    join(buffersize_filter(normal_bench, 40, 3, 2.0, 0.7), normal_exp),
    join(buffersize_filter(normal_bench, 100, 2, 1.0, 0.09), normal_exp),
    join(buffersize_filter(normal_bench, 100, 3, 1.0, 0.3), normal_exp),
    join(buffersize_filter(normal_bench, 100, 3, 2.0, 0.3), normal_exp),
    join(buffersize_filter(normal_bench, 100, 3, 2.0, 0.7), normal_exp)
]
inc_drift_data_buffersize = [
    join(buffersize_filter(incr_drift_bench, 10, 2, 1.0, 0.09), normal_exp),
    join(buffersize_filter(incr_drift_bench, 10, 3, 1.0, 0.3), normal_exp),
    join(buffersize_filter(incr_drift_bench, 10, 3, 2.0, 0.3), normal_exp),
    join(buffersize_filter(incr_drift_bench, 10, 3, 2.0, 0.7), normal_exp),
    join(buffersize_filter(normal_bench, 25, 10, 0.2, 0.01), normal_exp),
    join(buffersize_filter(incr_drift_bench, 40, 2, 1.0, 0.09), normal_exp),
    join(buffersize_filter(incr_drift_bench, 40, 3, 1.0, 0.3), normal_exp),
    join(buffersize_filter(incr_drift_bench, 40, 3, 2.0, 0.3), normal_exp),
    join(buffersize_filter(incr_drift_bench, 40, 3, 2.0, 0.7), normal_exp),
    join(buffersize_filter(incr_drift_bench, 100, 2, 1.0, 0.09), normal_exp),
    join(buffersize_filter(incr_drift_bench, 100, 3, 1.0, 0.3), normal_exp),
    join(buffersize_filter(incr_drift_bench, 100, 3, 2.0, 0.3), normal_exp),
    join(buffersize_filter(incr_drift_bench, 100, 3, 2.0, 0.7), normal_exp)
]

def decayfactor_filter(data0, cmapsize, cmapno, buffersize, rebuildth):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[0], parr(p)[1], parr(p)[2], parr(p)[4]] == [cmapsize, cmapno, buffersize, rebuildth] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[3]) }
    return data2

normal_data_decayfactor = [
    join(decayfactor_filter(normal_bench, 10, 2, 30, 0.09), normal_exp),
    join(decayfactor_filter(normal_bench, 10, 2, 30, 0.3), normal_exp),
    join(decayfactor_filter(normal_bench, 10, 2, 70, 0.3), normal_exp),
    join(decayfactor_filter(normal_bench, 10, 2, 70, 0.7), normal_exp),
    join(decayfactor_filter(normal_bench, 40, 2, 30, 0.09), normal_exp),
    join(decayfactor_filter(normal_bench, 40, 2, 30, 0.3), normal_exp),
    join(decayfactor_filter(normal_bench, 40, 2, 70, 0.3), normal_exp),
    join(decayfactor_filter(normal_bench, 40, 2, 70, 0.7), normal_exp),
    join(decayfactor_filter(normal_bench, 100, 2, 30, 0.09), normal_exp),
    join(decayfactor_filter(normal_bench, 100, 2, 30, 0.3), normal_exp),
    join(decayfactor_filter(normal_bench, 100, 2, 70, 0.3), normal_exp),
    join(decayfactor_filter(normal_bench, 100, 2, 70, 0.7), normal_exp)
]
inc_drift_data_decayfactor = [
    join(decayfactor_filter(incr_drift_bench, 10, 2, 30, 0.09), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 10, 2, 30, 0.3), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 10, 2, 70, 0.3), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 10, 2, 70, 0.7), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 40, 2, 30, 0.09), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 40, 2, 30, 0.3), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 40, 2, 70, 0.3), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 40, 2, 70, 0.7), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 100, 2, 30, 0.09), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 100, 2, 30, 0.3), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 100, 2, 70, 0.3), normal_exp),
    join(decayfactor_filter(incr_drift_bench, 100, 2, 70, 0.7), normal_exp)
]

def rebuildth_filter(data0, cmapsize, cmapno, buffersize, decayfactor):
    data1 = { p : s for p, s in data0.items()
        if [parr(p)[0], parr(p)[1], parr(p)[2], parr(p)[3]] == [cmapsize, cmapno, buffersize, decayfactor] }
    data2 = { p : s for p, s in sorted(data1.items(), key=lambda itm: parr(itm[0])[4]) }
    return data2

normal_data_rebuildth = [
    join(rebuildth_filter(normal_bench, 10, 2, 30, 1.0), normal_exp),
    join(rebuildth_filter(normal_bench, 10, 2, 70, 2.0), normal_exp),
    join(rebuildth_filter(normal_bench, 40, 3, 30, 1.0), normal_exp),
    join(rebuildth_filter(normal_bench, 40, 3, 70, 2.0), normal_exp),
    join(rebuildth_filter(normal_bench, 40, 2, 30, 1.0), normal_exp),
    join(rebuildth_filter(normal_bench, 100, 2, 70, 2.0), normal_exp),
    join(rebuildth_filter(normal_bench, 100, 3, 30, 1.0), normal_exp),
    join(rebuildth_filter(normal_bench, 100, 3, 70, 2.0), normal_exp),
    join(rebuildth_filter(normal_bench, 100, 2, 30, 1.0), normal_exp)

]
inc_drift_data_rebuildth = [
    join(rebuildth_filter(incr_drift_bench, 10, 2, 30, 1.0), normal_exp),
    join(rebuildth_filter(incr_drift_bench, 10, 2, 70, 2.0), normal_exp),
    join(rebuildth_filter(incr_drift_bench, 40, 3, 30, 1.0), normal_exp),
    join(rebuildth_filter(incr_drift_bench, 40, 3, 70, 2.0), normal_exp),
    join(rebuildth_filter(incr_drift_bench, 40, 2, 30, 1.0), normal_exp),
    join(rebuildth_filter(incr_drift_bench, 100, 2, 70, 2.0), normal_exp),
    join(rebuildth_filter(incr_drift_bench, 100, 3, 30, 1.0), normal_exp),
    join(rebuildth_filter(incr_drift_bench, 100, 3, 70, 2.0), normal_exp),
    join(rebuildth_filter(incr_drift_bench, 100, 2, 30, 1.0), normal_exp)
]


# Plot

prefix = 'conf/'

plt.figure(figsize=(10, 10))
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
plt.plot(normal_data[0], normal_data[1], 'r+')
normal_conf = 'normal-conf'
plt.savefig(prefix + normal_conf + '.pdf')
plt.savefig(prefix + normal_conf + '.png')

plt.figure(figsize=(10, 10))
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
plt.plot(incr_drift_data[0], incr_drift_data[1], 'b+')
incr_drift_conf = 'incr-drift-conf'
plt.savefig(prefix + incr_drift_conf + '.pdf')
plt.savefig(prefix + incr_drift_conf + '.png')


# Plot for fixed: normal

plt.figure(figsize=(10, 10))
plt.title("cmapsize for normal")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in normal_data_cmapsize:
    plt.plot(d[0], d[1], 'r')
normal_cmapsize_name = 'normal-cmapsize'
plt.savefig(prefix + normal_cmapsize_name + '.pdf')
plt.savefig(prefix + normal_cmapsize_name + '.png')

plt.figure(figsize=(10, 10))
plt.title("cmapno for normal")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in normal_data_cmapno:
    plt.plot(d[0], d[1], 'r')
normal_cmapno_name = 'normal-cmapno'
plt.savefig(prefix + normal_cmapno_name + '.pdf')
plt.savefig(prefix + normal_cmapno_name + '.png')

plt.figure(figsize=(10, 10))
plt.title("buffersize for normal")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in normal_data_buffersize:
    plt.plot(d[0], d[1], 'r')
normal_buffersize_name = 'normal-buffersize'
plt.savefig(prefix + normal_buffersize_name + '.pdf')
plt.savefig(prefix + normal_buffersize_name + '.png')

plt.figure(figsize=(10, 10))
plt.title("decayfactor for normal")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in normal_data_decayfactor:
    plt.plot(d[0], d[1], 'r')
normal_decayfactor_name = 'normal-decayfactor'
plt.savefig(prefix + normal_decayfactor_name + '.pdf')
plt.savefig(prefix + normal_decayfactor_name + '.png')

plt.figure(figsize=(10, 10))
plt.title("rebuildth for normal")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in normal_data_rebuildth:
    plt.plot(d[0], d[1], 'r')
normal_rebuildth_name = 'normal-rebuildth'
plt.savefig(prefix + normal_rebuildth_name + '.pdf')
plt.savefig(prefix + normal_rebuildth_name + '.png')


# Plot for fixed: incr drift

plt.figure(figsize=(10, 10))
plt.title("cmapsize for incr drift")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in inc_drift_data_cmapsize:
    plt.plot(d[0], d[1], 'b')
incr_drift_cmapsize_name = 'incr-drift-cmapsize'
plt.savefig(prefix + incr_drift_cmapsize_name + '.pdf')
plt.savefig(prefix + incr_drift_cmapsize_name + '.png')

plt.figure(figsize=(10, 10))
plt.title("cmapno for incr drift")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in inc_drift_data_cmapno:
    plt.plot(d[0], d[1], 'b')
incr_drift_cmapno_name = 'incr-drift-cmapno-name'
plt.savefig(prefix + incr_drift_cmapno_name + '.pdf')
plt.savefig(prefix + incr_drift_cmapno_name + '.png')

plt.figure(figsize=(10, 10))
plt.title("buffersize for incr drift")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in inc_drift_data_buffersize:
    plt.plot(d[0], d[1], 'b')
incr_drift_buffersize_name = 'incr-drift-buffersize'
plt.savefig(prefix + incr_drift_buffersize_name + '.pdf')
plt.savefig(prefix + incr_drift_buffersize_name + '.png')

plt.figure(figsize=(10, 10))
plt.title("decayfactor for incr drift")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in inc_drift_data_decayfactor:
    plt.plot(d[0], d[1], 'b')
incr_drift_decayfactor_name = 'incr-drift-decayfactor'
plt.savefig(prefix + incr_drift_decayfactor_name + '.pdf')
plt.savefig(prefix + incr_drift_decayfactor_name + '.png')

plt.figure(figsize=(10, 10))
plt.title("rebuildth for incr drift")
plt.xlim((0, 1.8))
plt.ylim((0, 1000000))
for d in inc_drift_data_rebuildth:
    plt.plot(d[0], d[1], 'b')
incr_drift_rebuildth_name = 'incr-drift-rebuildth'
plt.savefig(prefix + incr_drift_rebuildth_name + '.pdf')
plt.savefig(prefix + incr_drift_rebuildth_name + '.png')

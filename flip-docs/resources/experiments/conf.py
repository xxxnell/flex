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

files = [f for f in listdir(benchDir) if isfile(join(benchDir, f))]
last_file_dir = bench_dir + sorted(files)[-1]

bench_raw = prep.data_str(last_file_dir)
normal_raw = list(filter(lambda d: d[0] == "flip.benchmark.ConfBench.normal", bench_raw))
incr_drift_raw = list(filter(lambda d: d[0] == "flip.benchmark.ConfBench.incrDrift", bench_raw))

def bench_data(init, raw):
    norgx = "([-+]?[0-9]*\.?[0-9]*)"
    for d in raw:
        cmap_size = re.compile(r"cmapSizeLS -> " + norgx).search(d[1]).group(1)
        cmap_no = re.compile(r"cmapNoLS -> " + norgx).search(d[1]).group(1)
        buffer_size = re.compile(r"bufferSizeLS -> " + norgx).search(d[1]).group(1)
        decay_factor = re.compile(r"decayFactorLS -> " + norgx).search(d[1]).group(1)
        rebuild_threshold = re.compile(r"rebuildThresholdLS -> " + norgx).search(d[1]).group(1)
        score = float(re.compile(r"iterateBenchSizeLS -> " + norgx).search(d[1]).group(1)) * float(d[2])
        init[repr([cmap_size, cmap_no, buffer_size, decay_factor, rebuild_threshold])] = score
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
        score = bench_data.get(params)
        error = exp_data.get(params)
        if score and error :
            x.append(error)
            y.append(score)
    return [x, y]
    
normal_data = join(normal_bench, normal_exp)
incr_drift_data = join(incr_drift_bench, incr_drift_exp)

plt.figure(figsize=(10, 10))
plt.plot(normal_data[0], normal_data[1], 'r+')
plt.plot(incr_drift_data[0], incr_drift_data[1], 'b+')
plt.show()

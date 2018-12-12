import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rcParams
from matplotlib import cm
from matplotlib import colors
import preprocessing as prep
import barplot

"""
# Constants
"""
fontsize = 22
figsize1=(10, 5.5)
figsize2=(8, 5.5)
prefix = 'performance/'
rcParams['font.family'] = 'serif'
rcParams.update({'font.size': fontsize})
plt.set_cmap('viridis')
norm = colors.Normalize(vmin=0, vmax=256.0)
m = cm.ScalarMappable(norm=norm, cmap=plt.get_cmap('viridis') )


"""
Update Throughput
"""
label = ['oKDE', 'SPDT', 'SPDTw', 'FlexSketch']
color = [m.to_rgba(210), m.to_rgba(180), m.to_rgba(120), m.to_rgba(50)]
stat_update_thrp = [69.0, 65.5, 0.590, 1081]
nonstat_update_thrp = [64.9, 65.3, 0.654, 374]
mixture_update_thrp = [63.4, 65.9, 0.601, 612]
stat_update_thrp_m = [score / 1000 for score in stat_update_thrp]
nonstat_update_thrp_m = [score / 1000 for score in nonstat_update_thrp]
mixture_update_thrp_m = [score / 1000 for score in mixture_update_thrp]
data = [stat_update_thrp_m, nonstat_update_thrp_m, mixture_update_thrp_m]

fig, ax = plt.subplots(figsize=figsize1)
barplot.bar_plot(ax, data, label, color, (0, 1.200), fontsize=fontsize*0.9)

ax.set_ylabel('Throughput (Mops)')
ax.set_xticks(np.arange(len(data)) + barplot.bar_width(len(data[0])) * 3/2)
ax.set_xticklabels(['Stationary', 'Non-stationary', 'Mixture'])
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.90)

fig.tight_layout()
update_name = 'update-throughput'
plt.savefig(prefix + update_name + '.pdf')
plt.savefig(prefix + update_name + '.png')


"""
Probability Throughput
"""
margin = 0.2
label = ['oKDE', 'SPDT', 'SPDTw', 'FlexSketch', 'FlexSketch + cache']
color = [m.to_rgba(210), m.to_rgba(180), m.to_rgba(120), m.to_rgba(50), m.to_rgba(30)]
prob_thrp = [3546, 448, 442, 466, 4131]
prob_thrp_m = [score / 1000 for score in prob_thrp]
data = [prob_thrp_m]

fig, ax = plt.subplots(figsize=figsize1)
barplot.bar_plot(ax, data, label, color, (0, 5.000), fontsize=fontsize)

bw = barplot.bar_width(len(data[0]))
ax.set_ylabel('Throughput (Mops)')
ax.set_xticks(np.arange(len(data)) + bw * (len(data[0]) / 2 - 0.5))
ax.set_xticklabels([])
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.90)
ax.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))

fig.tight_layout()
prob_name = 'probability-throughput'
plt.savefig(prefix + prob_name + '.pdf')
plt.savefig(prefix + prob_name + '.png')


"""
Memory usage
"""
margin = 0.2
label = ['oKDE', 'SPDT', 'SPDTw', 'FlexSketch']
color = [m.to_rgba(210), m.to_rgba(180), m.to_rgba(120), m.to_rgba(50)]
mem = [4632, 4296, (4296 + 4816), 6192]
mem_k = [score / 1000 for score in mem]
data = [mem_k]

fig, ax = plt.subplots(figsize=figsize2)
barplot.bar_plot(ax, data, label, color, (0, 10.0), fontsize=fontsize)

bw = barplot.bar_width(len(data[0]))
ax.set_ylabel('Memory (KB)')
ax.set_xticks(np.arange(len(data)) + bw * (len(data[0]) / 2 - 0.5))
ax.set_xticklabels([])
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.9)
ax.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))

fig.tight_layout()
memory_name = 'memory-usage'
plt.savefig(prefix + memory_name + '.pdf')
plt.savefig(prefix + memory_name + '.png')

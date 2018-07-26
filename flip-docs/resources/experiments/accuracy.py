import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import preprocessing as prep
from matplotlib import rcParams
from matplotlib import cm
from matplotlib import colors
import preprocessing as prep
import barplot

"""
# Constants
"""
fontsize = 18
figsize = (6,5)
margin = 0.2
inp_prefix = 'data/'
out_prefix = 'accuracy/'
rcParams['font.family'] = 'serif'
rcParams.update({'font.size': fontsize})
plt.set_cmap('viridis')
norm = colors.Normalize(vmin=0, vmax=256.0)
m = cm.ScalarMappable(norm=norm, cmap=plt.get_cmap('viridis') )

"""
# Stationary
"""

"""
# Normal Distribution
"""
label = ['oKDE', 'SPDT', 'SPDTw', 'D-Sketch']
color = [m.to_rgba(210), m.to_rgba(190), m.to_rgba(120), m.to_rgba(50)]
normal_ed = [0.00910, 0.0145, 0.0393, 0.0122]
data = [normal_ed]

fig, ax = plt.subplots(figsize=figsize)
barplot.bar_plot(ax, data, label, color, (0, 0.055), fontsize=fontsize)

bw = barplot.bar_width(len(data[0]))
ax.set_ylabel('Statistical distance ($D_{\Delta})$')
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.85)
ax.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))
ax.set_xticklabels([])

fig.tight_layout()
update_name = 'normal-accuracy'
plt.savefig(out_prefix + update_name + '.pdf')
plt.savefig(out_prefix + update_name + '.png')
plt.show()


"""
# Bimodal Distribution
"""

bimodal_ed = [0.0178, 0.0222, 0.0453, 0.0181]
data = [bimodal_ed]
label = ['oKDE', 'SPDT', 'SPDTw', 'D-Sketch']
color = [m.to_rgba(210), m.to_rgba(190), m.to_rgba(120), m.to_rgba(50)]

fig, ax = plt.subplots(figsize=figsize)
barplot.bar_plot(ax, data, label, color, (0, 0.055), fontsize=fontsize)

bw = barplot.bar_width(len(data[0]))
ax.set_ylabel('Statistical distance ($D_{\Delta})$')
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.85)
ax.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))
ax.set_xticklabels([])

fig.tight_layout()
bimodal_name = 'bimodal-accuracy'
plt.savefig(out_prefix + bimodal_name + '.pdf')
plt.savefig(out_prefix + bimodal_name + '.png')

"""
# Lognormal Distribution
"""
lognormal_ed = [0.0342, 0.0230, 0.0504, 0.0247]
data = [lognormal_ed]
label = ['oKDE', 'SPDT', 'SPDTw', 'D-Sketch']
color = [m.to_rgba(210), m.to_rgba(190), m.to_rgba(120), m.to_rgba(50)]

fig, ax = plt.subplots(figsize=figsize)
barplot.bar_plot(ax, data, label, color, (0, 0.055), fontsize=fontsize)

bw = barplot.bar_width(len(data[0]))
ax.set_ylabel('Statistical distance ($D_{\Delta})$')
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.85)
ax.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))
ax.set_xticklabels([])

fig.tight_layout()
lognormal_name = 'lognormal-accuracy'
plt.savefig(out_prefix + lognormal_name + '.pdf')
plt.savefig(out_prefix + lognormal_name + '.png')

"""
# Combinde Stationary
"""
label = ['oKDE', 'SPDT', 'SPDTw', 'D-Sketch']
color = [m.to_rgba(210), m.to_rgba(190), m.to_rgba(120), m.to_rgba(50)]
data = [normal_ed, bimodal_ed, lognormal_ed]

fig, ax = plt.subplots(figsize=figsize)
barplot.bar_plot(ax, data, label, color, (0, 0.055), fontsize=fontsize)

bw = barplot.bar_width(len(data[0]))
ax.set_ylabel('Statistical distance ($D_{\Delta}$)')
ax.set_xticks(np.arange(len(data)) + barplot.bar_width(len(data[0])) * 3/2)
ax.set_xticklabels(['Normal', 'Bimodal', 'Log-normal'])
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.85)
# ax.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))

fig.tight_layout()
stationary_name = 'stationary-accuracy'
plt.savefig(out_prefix + stationary_name + '.pdf')
plt.savefig(out_prefix + stationary_name + '.png')


"""
# Non-stationary
"""
figsize = (6,5)

def distplot(ax, tdata, ddata, color, label, linestyle):
    for i in range(len(tdata)):
        ax.plot(tdata[i], ddata[i], color=color[i], label=label[i], linestyle=linestyle[i], linewidth=1.5, alpha=0.8)
    return ax

def avg(l):
    return sum(l) / float(len(l))

"""
# Incremental Concept Drift
"""
cd_start = 300
avg_start = 500
avg_end = 1000
speed = 0.00308

data_loc = inp_prefix + "okde-incr-cd-ed.out"
okde_data_raw = { d[0]: d[1] for d in prep.data(data_loc, 2) }
data_loc = inp_prefix + "spdt-incr-cd-ed.out"
spdt_data_raw = { d[0]: d[1] for d in prep.data(data_loc, 2) }
data_loc = inp_prefix + "sketch-incr-cd-ed.out"
sketch_data_raw = { d[0]: d[1] for d in prep.data(data_loc, 2) }

incr_ed = [
    avg([dist for i, dist in okde_data_raw.items() if i >= avg_start if i <= avg_end]),
    avg([dist for i, dist in spdt_data_raw.items() if i >= avg_start if i <= avg_end]),
    avg([dist for i, dist in sketch_data_raw.items() if i >= avg_start if i <= avg_end])]
ck = [ed / speed for ed in incr_ed]


"""
  Plot
"""
label = ['oKDE', 'SPDTw', 'D-Sketch']
color = [m.to_rgba(210), m.to_rgba(120), m.to_rgba(50)]
data = [ck]

fig, ax = plt.subplots(figsize=figsize)
ax = barplot.bar_plot(ax, data, label, color, (0, 210), fontsize=fontsize)

bw = barplot.bar_width(len(data[0]))
ax.set_ylabel('$c/k$')
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.85)
ax.set_xticks(np.arange(len(data)) + bw * (len(data[0]) / 2 - 0.5))
ax.set_xticklabels([])
ax.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))

fig.tight_layout()
incr_cd_name = 'incr-cd-coeff'
plt.savefig(out_prefix + incr_cd_name + '.pdf')
plt.savefig(out_prefix + incr_cd_name + '.png')
plt.show()

"""
# Incremental Concept Drift: D_{\Delta} over time
"""
start = 250
end = 700

data_loc = inp_prefix + "okde-incr-cd-ed.out"
okde_ed_data = [[dt[0], dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] <= end]
okde_tdata = list(map(lambda d: d[0], okde_ed_data))
okde_ddata = list(map(lambda d: d[1] , okde_ed_data))

data_loc = inp_prefix + "spdt-incr-cd-ed.out"
spdt_ed_data = [[dt[0], dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] <= end]
spdt_tdata = list(map(lambda d: d[0], spdt_ed_data))
spdt_ddata = list(map(lambda d: d[1] , spdt_ed_data))

data_loc = inp_prefix + "sketch-incr-cd-ed.out"
sketch_ed_data = [[dt[0], dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] <= end]
sketch_tdata = list(map(lambda d: d[0], sketch_ed_data))
sketch_ddata = list(map(lambda d: d[1] , sketch_ed_data))

fig, ax = plt.subplots(figsize=figsize)
distplot(ax, [okde_tdata, spdt_tdata, sketch_tdata], [okde_ddata, spdt_ddata, sketch_ddata], color, label, ['--', '-.', '-'])

ax.set_xlim(xmin=start, xmax=end)
ax.set_ylim(ymin=0)
ax.set_ylabel('Statistical distance ($D_{\Delta}$)')
ax.set_xlabel('timestamp ($t$)')
ax.tick_params(axis='x', labelsize=fontsize * 0.85)
ax.tick_params(axis='y', labelsize=fontsize * 0.85)
ax.legend(edgecolor='black', fancybox=False)

update_name = 'incr-cd-dist'
plt.savefig(out_prefix + update_name + '.pdf')
plt.savefig(out_prefix + update_name + '.png')
plt.show()


"""
# Sudden Concept Drift
"""
def func3(t, l, ck, a1, a2):
    w = c2w(l, ck)
    return a1 * np.exp((-1) * (l + w) * t) + a2 * np.exp((-1) * (l - w) * t)

def func3ck(ck):
    return (lambda t, l, a1, a2: func3(t, l, ck, a1, a2))

def c2w(l, ck):
    k = 2 * l / ck
    return (l*l - k)**(.5)

"""
# oKDE
"""
start = 300
end = 500

data_loc = "data/okde-sudden-cd-ed.out"
ed_data = [[dt[0] - start, dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] < end]

tdata = list(map(lambda d: d[0], ed_data))
ddata = list(map(lambda d: d[1] , ed_data))

plt.plot(tdata, ddata, 'b-', label='data')
plt.xlim(xmin = 0, xmax = end - start)
plt.ylim(ymax = 1.0, ymin = 0)

okde_coeff, pcov = curve_fit(func3ck(ck[0]), tdata, ddata, (0.8, 1.0, 0))
print("oKDE coefficients (l, a1, a2)" )
print(okde_coeff)

tdata = np.linspace(0, end - start, 300)
ddata = func3ck(ck[0])(tdata, *okde_coeff)

plt.plot(tdata, ddata, 'r-')
plt.show()

"""
# SPDTw
"""
start = 300
end = 450

data_loc = "data/spdt-sudden-cd-ed.out"
ed_data = [[dt[0] - start, dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] < end]

tdata = list(map(lambda d: d[0], ed_data))
ddata = list(map(lambda d: d[1] , ed_data))

plt.plot(tdata, ddata, 'b-', label='data')
plt.xlim(xmin = 0, xmax = end - start)
plt.ylim(ymax = 2.0, ymin = 0)

spdtw_coeff, pcov = curve_fit(func3ck(ck[1]), tdata, ddata, (0.25, 1.5, 0))
print("SPDTw coefficients (l, a1, a2)" )
print(spdtw_coeff)

tdata = np.linspace(0, end - start, 300)
ddata = func3ck(ck[1])(tdata, *spdtw_coeff)

plt.plot(tdata, ddata, 'r-')
plt.show()

"""
# Sketch
"""
start = 300
end = 450

data_loc = "data/sketch-sudden-cd-ed.out"
ed_data = [[dt[0] - start, dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] < end]

tdata = list(map(lambda d: d[0], ed_data))
ddata = list(map(lambda d: d[1] , ed_data))

plt.plot(tdata, ddata, 'b-', label='data')
plt.xlim(xmin = 0, xmax = end - start)
plt.ylim(ymax = 1.5, ymin = 0)

sketch_coeff, pcov = curve_fit(func3ck(ck[2]), tdata, ddata, (0.25, 1.5, 0))
# sketch_coeff = (0.25, 1.5, 0)
print("sketch coefficients (l, a1, a2)")
print(sketch_coeff)

tdata = np.linspace(0, end - start, 300)
ddata = func3ck(ck[2])(tdata, *sketch_coeff)

plt.plot(tdata, ddata, 'r-')
plt.show()

"""
# Sudden Concept Drift Index Plot
"""
label = ['oKDE', 'SPDTw', 'D-Sketch']
color = [m.to_rgba(210), m.to_rgba(120), m.to_rgba(50)]
hl = [3, 35, 45]
l = [okde_coeff[0], spdtw_coeff[0], sketch_coeff[0]]
om = [c2w(l[i], ck[i]) for i in range(0, 3)]
tau1 = [1 / (l[i] + om[i]) for i in range(0, 3)]
tau2 = [1 / (l[i] - om[i]) for i in range(0, 3)]
data1 = [hl]
data2 = [tau2]

print("l: " + ', '.join(str(x) for x in l))
print("om: " + ', '.join(str(x) for x in om))
print("tau1: " + ', '.join(str(x) for x in tau1))
print("tau2: " + ', '.join(str(x) for x in tau2))

fig, (ax1, ax2) = plt.subplots(figsize=(8, 5), ncols=2)
plt.subplots_adjust(wspace=0.0, hspace=0.0)

barplot.bar_plot(ax1, data1, label, color, (0, 50.0), fontsize=fontsize, legend=False)
ax1.set_ylabel('$t_{1/2}$')
ax1.set_xticklabels([])
ax1.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))

barplot.bar_plot(ax2, data2, label, color, (0, 300.0), fontsize=fontsize)
ax2.set_ylabel('$\\tau$')
ax2.set_xticklabels([])
ax2.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))
ax2.yaxis.set_label_position("right")
ax2.yaxis.tick_right()

sudden_cd_name = 'sudden-cd-coeff'
plt.savefig(out_prefix + sudden_cd_name + '.pdf')
plt.savefig(out_prefix + sudden_cd_name + '.png')
plt.show()

"""
# Sudden Concept Drift: D_{\Delta} over time
"""
start = 250
end = 800

data_loc = inp_prefix + "sketch-sudden-cd-ed.out"
sketch_ed_data = [[dt[0], dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] <= end]
sketch_tdata = list(map(lambda d: d[0], sketch_ed_data))
sketch_ddata = list(map(lambda d: d[1] - 0.8 * normal_ed[2], sketch_ed_data))

data_loc = inp_prefix + "okde-sudden-cd-ed.out"
okde_ed_data = [[dt[0], dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] <= end]
okde_tdata = list(map(lambda d: d[0], okde_ed_data))
okde_ddata = list(map(lambda d: d[1] - 0.8 * normal_ed[0], okde_ed_data))

data_loc = inp_prefix + "spdt-sudden-cd-ed.out"
spdt_ed_data = [[dt[0], dt[1]] for dt in prep.data(data_loc, 2) if dt[0] >= start if dt[0] <= end]
spdt_tdata = list(map(lambda d: d[0], spdt_ed_data))
spdt_ddata = list(map(lambda d: d[1] - 0.8 * normal_ed[1], spdt_ed_data))

fig, (ax1, ax2) = plt.subplots(figsize=(8, 5), ncols=2)
plt.subplots_adjust(wspace=0.05, hspace=0.0)
distplot(ax1, [okde_tdata, spdt_tdata, sketch_tdata], [okde_ddata, spdt_ddata, sketch_ddata], color, label, ['--', '-.', '-'])
distplot(ax2, [okde_tdata, spdt_tdata, sketch_tdata], [okde_ddata, spdt_ddata, sketch_ddata], color, label, ['--', '-.', '-'])

ax1.set_xlim(xmin=250, xmax=400)
ax1.set_ylim(ymin=0, ymax=1.3)
ax2.set_xlim(xmin=650, xmax=800)
ax2.set_ylim(ymin=0, ymax=0.1)
ax1.set_ylabel("Statistical distance ($D_{\Delta})$")
fig.text(0.5, 0.02, 'timestamp ($t$)', ha='center')
ax1.xaxis.set_ticks([300, 400])
ax2.xaxis.set_ticks([700, 800])
ax2.yaxis.set_label_position("right")
ax2.yaxis.tick_right()
ax2.legend(edgecolor='black', fancybox=False)

d = .015
kwargs = dict(transform=ax1.transAxes, color='black', clip_on=False, linewidth=1.0)
ax1.plot((1-d,1+d),(1-d,1+d), **kwargs)
ax1.plot((1-d,1+d),(-d,+d), **kwargs)
ax1.spines['right'].set_visible(False)
kwargs.update(transform=ax2.transAxes)
ax2.plot((-d,+d),(1-d,1+d), **kwargs)
ax2.plot((-d,+d),(-d,+d), **kwargs)
ax2.spines['left'].set_visible(False)


update_name = 'sudden-cd-dist'
plt.savefig(out_prefix + update_name + '.pdf')
plt.savefig(out_prefix + update_name + '.png')
plt.show()


"""
# Blip Concept Drift
"""
start = 300
end = 303

data_loc = inp_prefix + "okde-blip-cd-ed.out"
okde_data_raw = { d[0]: d[1] for d in prep.data(data_loc, 2) }
data_loc = inp_prefix + "spdt-blip-cd-ed.out"
spdt_data_raw = { d[0]: d[1] for d in prep.data(data_loc, 2) }
data_loc = inp_prefix + "sketch-blip-cd-ed.out"
sketch_data_raw = { d[0]: d[1] for d in prep.data(data_loc, 2) }


"""
# Plot
"""
label = ['oKDE', 'SPDTw', 'D-Sketch']
color = [m.to_rgba(210), m.to_rgba(120), m.to_rgba(50)]
edst = [okde_data_raw[start], spdt_data_raw[start], sketch_data_raw[start]]
edfn = [okde_data_raw[end], spdt_data_raw[end], sketch_data_raw[end]]
dur = end - start
vols = [((edfn[i] - edst[i]) / dur) for i in range(len(edst))]

print("volatility: " + ', '.join(map(str, vols)))

fig, ax = plt.subplots(figsize=figsize)
barplot.bar_plot(ax, [vols], label, color, (0, 0.35), fontsize=fontsize)

ax.set_ylabel('$\sigma$')
ax.tick_params(axis='x', which='both', bottom=False, top=False)
ax.tick_params(axis='y', labelsize=fontsize * 0.85)
ax.set_xticklabels([])
ax.set_xlim((-bw / 2 - margin, bw * (len(data[0]) - 0.5) + margin))

fig.tight_layout()
update_name = 'blip-cd-coeff'
plt.savefig(out_prefix + update_name + '.pdf')
plt.savefig(out_prefix + update_name + '.png')
plt.show()

"""
# Blip Concept Drift: D_{\Delta} over time
"""
start = 298
end = 306

okde_ed_data = [[i, dist] for i, dist in okde_data_raw.items() if i >= start if i <= end]
okde_tdata = list(map(lambda d: d[0], okde_ed_data))
okde_ddata = list(map(lambda d: d[1], okde_ed_data))

spdt_ed_data = [[i, dist] for i, dist in spdt_data_raw.items() if i >= start if i <= end]
spdt_tdata = list(map(lambda d: d[0], spdt_ed_data))
spdt_ddata = list(map(lambda d: d[1], spdt_ed_data))

sketch_ed_data = [[i, dist] for i, dist in sketch_data_raw.items() if i >= start if i <= end]
sketch_tdata = list(map(lambda d: d[0], sketch_ed_data))
sketch_ddata = list(map(lambda d: d[1], sketch_ed_data))

fig, ax = plt.subplots(figsize=figsize)
distplot(ax, [okde_tdata, spdt_tdata, sketch_tdata], [okde_ddata, spdt_ddata, sketch_ddata], color, label, ['--', '-.', '-'])

plt.xlim(xmin=start, xmax=end)
plt.ylim(ymin=0)
plt.ylabel('Statistical distance ($D_{\Delta})$')
plt.xlabel('timestamp ($t$)')
ax.tick_params(axis='x', which='both', top=False, labelsize=fontsize * 0.85)
ax.tick_params(axis='y', labelsize=fontsize * 0.85)
ax.legend(edgecolor='black', fancybox=False)

update_name = 'blip-cd-dist'
plt.savefig(out_prefix + update_name + '.pdf')
plt.savefig(out_prefix + update_name + '.png')
plt.show()

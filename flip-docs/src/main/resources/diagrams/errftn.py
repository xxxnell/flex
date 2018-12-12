import matplotlib.pyplot as plt
from matplotlib import rcParams
from matplotlib import cm
from matplotlib import colors
import scipy.stats as stats
import numpy as np

def edf(xs, data):
    _data = sorted(data)
    n = len(_data)
    i = 0
    m = 0
    edf = []
    for a in _data:
        o = len([x for x in xs if a > x])
        edf = edf + [i / n] * (o - m)
        i += 1
        m = o
    edf = edf + [1] * (len(xs) - len(edf))
    return edf

def pd(xs, delta):
    pd = []
    x1 = xs[0]
    for x2 in xs[1:]:
        pd = pd + [len([d for d in delta if d >= x1 and d < x2])]
        x1 = x2
    pd = [d / len(delta) for d in pd]
    return pd

# Params
fontsize = 22
rcParams['font.family'] = 'serif'
rcParams.update({'font.size': fontsize})
figsize = (6,5)
plt.set_cmap('viridis')
norm = colors.Normalize(vmin=0, vmax=256.0)
m = cm.ScalarMappable(norm=norm, cmap=plt.get_cmap('viridis') )
# color = [m.to_rgba(210), m.to_rgba(190), m.to_rgba(120), m.to_rgba(50)]
color = [m.to_rgba(210), m.to_rgba(120), m.to_rgba(50)]

# Data

mu1 = 0.0
mu2 = 0.5
sigma = 1
size = 10
x = np.linspace(min([mu1, mu2]) - 3 * sigma, max([mu1, mu2]) + 3 * sigma, 300)
cdf = stats.norm.cdf(x)
data = np.random.normal(mu2, sigma, size)
linewidth = 2

# EDF
edf = edf(x, data)

# Delta
delta = [abs(d) for d in edf - cdf]

# Plot Delta
fig, ax = plt.subplots(figsize=figsize)

plt.plot(x, cdf, label='$CDF_{\mathcal{M}}$', color=color[0], linestyle='--', linewidth=linewidth)
plt.plot(x, edf, label='$EDF_{X}$', color=color[1], linestyle='-.', linewidth=linewidth)
plt.plot(x, delta, label='$\Delta$', color=color[2], linewidth=linewidth)
plt.xlabel('$x$')
plt.ylabel('CDF')
plt.xlim(left=-2.5, right=2.5)
plt.ylim(top=1.005, bottom=-0.005)
ax.legend(edgecolor='black', fancybox=False, fontsize=fontsize*0.9)

delta_name = 'delta'
plt.savefig(delta_name + '.pdf', bbox_inches='tight')
plt.savefig(delta_name + '.png', bbox_inches='tight')
plt.show()

# p(Δ)
_dx = np.linspace(0, 1, 100)
dx = _dx[1:]
pd = [x * 2 for x in pd(_dx, delta)]


eps_max = max(delta)
eps_avg = sum(delta) / float(len(delta))

# Plot p(Δ)

fig, ax = plt.subplots(figsize=figsize)

ax.annotate("max",
            xy=(eps_max, 0.03), xycoords='data',
            xytext=(eps_max, 0.13), textcoords='data',
            size=fontsize, va="center", ha="center",
            arrowprops=dict(arrowstyle="->"),)

ax.annotate("mean",
            xy=(eps_avg, 0.1), xycoords='data',
            xytext=(eps_avg, 0.2), textcoords='data',
            size=fontsize, va="center", ha="center",
            arrowprops=dict(arrowstyle="->"))

plt.plot(dx, pd, color=color[2], linewidth=linewidth)
plt.xlabel('$\Delta$')
plt.ylabel('$p(\Delta)$')
plt.xlim(left=0, right=0.5)
plt.ylim(bottom=0, top=0.4)

pdelta_name = 'pdelta'
plt.savefig(pdelta_name + '.pdf', bbox_inches='tight')
plt.savefig(pdelta_name + '.png', bbox_inches='tight')
plt.show()

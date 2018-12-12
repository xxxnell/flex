import matplotlib.pyplot as plt
from matplotlib import rcParams
from matplotlib import cm
from matplotlib import colors
import scipy.stats as stats
import numpy as np

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
linewidth = 2

def sudden(x):
    if(x < 300):
        res = 0
    else:
        res = 5
    return res

def incr(x):
    if(x < 300):
        res = 0
    else:
        res = (x - 300) * 0.01
    return res

def blip(x):
    if(x > 300 and x <= 303):
        res = 5
    else:
        res = 0
    return res

ts1 = range(200, 400)
sudden = [sudden(t) for t in ts1]
ts2 = range(200, 400)
incr = [incr(t) for t in ts2]
ts3 = range(200, 400)
blip = [blip(t) for t in ts3]

fig, ax = plt.subplots(figsize=figsize)
plt.plot(ts1, sudden, color=color[0], linewidth=linewidth)
plt.xlabel('Timestamp ($t$)')
plt.ylabel('$x(t)$')
plt.xlim(left=250, right=350)

sudden_name = 'sudden-x'
plt.savefig(sudden_name + '.pdf', bbox_inches='tight')
plt.savefig(sudden_name + '.png', bbox_inches='tight')
plt.show()

fig, ax = plt.subplots(figsize=figsize)
plt.plot(ts2, incr, color=color[1], linewidth=linewidth)
plt.xlabel('Timestamp ($t$)')
plt.ylabel('$x(t)$')
plt.xlim(left=250, right=350)
plt.ylim(bottom=-0.02, top=0.52)

sudden_name = 'incr-x'
plt.savefig(sudden_name + '.pdf', bbox_inches='tight')
plt.savefig(sudden_name + '.png', bbox_inches='tight')
plt.show()

fig, ax = plt.subplots(figsize=figsize)
plt.plot(ts3, blip, color=color[2], linewidth=linewidth)
plt.xlabel('Timestamp ($t$)')
plt.ylabel('$x(t)$')
plt.xlim(left=250, right=350)

sudden_name = 'blip-x'
plt.savefig(sudden_name + '.pdf', bbox_inches='tight')
plt.savefig(sudden_name + '.png', bbox_inches='tight')
plt.show()

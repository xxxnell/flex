import numpy as np
import matplotlib.pyplot as plt
import preprocessing as prep


def bar_label(ax, rects, margin, fontsize):
    """
    Attach a text label above each bar displaying its height
    """
    for rect in rects:
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width() / 2., height + margin,
                prep.precision(height, 2),
                ha='center', va='bottom', fontsize=fontsize)


def bar_width(size):
    return 1 / (size + 1.0)


def bar_plot(ax, datas, label, color, ylim, fontsize=18, opacity=0.8, legend=True, legendloc=0):
    index = np.arange(len(datas))
    datast = np.transpose(datas)

    for i in range(0, len(datast)):
        bw = bar_width(len(datast))
        rects = ax.bar(index + i * bw, datast[i], bw,
            alpha=opacity, label=label[i], edgecolor='black', linewidth=1.0, color=color[i])
        bar_label(ax, rects, (ylim[1] - ylim[0]) * 0.01, fontsize * 0.80)

    ax.tick_params(axis='x', which='both', bottom=False, top=False)
    ax.tick_params(axis='y', labelsize=fontsize * 0.85)
    if legend:
        legend = ax.legend(edgecolor='black', fancybox=False, loc=legendloc)
    ax.set_ylim(ylim)

    return ax

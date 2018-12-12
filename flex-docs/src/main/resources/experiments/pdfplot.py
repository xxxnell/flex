import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import preprocessing as prep
from scipy.stats import norm


# basic frame

def pdfframe(ax, xmin, xmax, ymin, ymax):
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)
    ax.set_ylabel("probability density")
    ax.set_xlabel("x")


# dashed

def empty_pdf_dashed_dataframe(ax):
    return ax.plot([], [], '--', color='orange')

def setline_for_expected(lines, expected, xmin, xmax):
    margin = 10
    line, = lines
    x2 = np.arange(xmin - margin, xmax + margin, 0.1)
    y2 = expected(x2)

    line.set_data(x2, y2)

def pdfframe_with_expected(ax, expected, xmin, xmax, ymin, ymax):
    pdfframe(ax, xmin, xmax, ymin, ymax)
    line = empty_pdf_dashed_dataframe(ax)
    setline_for_expected(line, expected, xmin, xmax)


# errorbar

def empty_pdf_errorbar_dataframe(ax):
    x1, xerr1, y1 = ([0], [0], [0])
    err = ax.errorbar(x1, y1, xerr=xerr1, fmt='.', color='royalblue')

    return err


def seterr(err, x_base, xerr, y_base):
    data_line, caplines, (barlinecol,) = err.lines

    xerr_top = np.array(x_base) + np.array(xerr)
    xerr_bot = np.array(x_base) - np.array(xerr)

    data_line.set_data(x_base, y_base)
    new_segments_x = [np.array([[xt, y], [xb,y]]) for xt, xb, y in zip(xerr_top, xerr_bot, y_base)]
    barlinecol.set_segments(new_segments_x)

    return err


def seterr_for_loc(err, data_loc, xmin, xmax):
    x_base, xerr, y_base = prep.data_loc_to_data(data_loc, xmin, xmax)
    return seterr(err, x_base, xerr, y_base)


def pdfplot_errorbar(ax, expected, data_loc, xmin, xmax, ymin, ymax, notation = None):
    pdfframe_with_expected(ax, expected, xmin, xmax, ymin, ymax)
    err = empty_pdf_errorbar_dataframe(ax, notation)
    seterr_for_loc(err, data_loc, xmin, xmax)


def animated_pdfplot_errorbar(data_loc, start, end, step, expected, xmin, xmax, ymin, ymax):
    fig, ax = plt.subplots()

    pdfframe(ax, xmin, xmax, ymin, ymax)
    lines = empty_pdf_dashed_dataframe(ax)
    err = empty_pdf_errorbar_dataframe(ax)

    def animate(i):
        setline_for_expected(lines, lambda x: expected(x, i), xmin, xmax)
        seterr_for_loc(err, data_loc(i), xmin, xmax)
        ax.set_title("estimated pdf for update count: " + str(i))
        return err,

    idxs = np.arange(start, end, step)
    return animation.FuncAnimation(fig, animate, idxs, repeat=False, blit=False)


# bar

def init_pdf_bar_dataframe(ax, x_base, xerr, y_base):
    height = y_base
    width = [i * 2 for i in xerr]

    bars = ax.bar(x_base, height, width = width, align = 'center', edgecolor=['black']*len(x_base))

    return bars


def init_pdf_bar_dataframe_for_loc(ax, data_loc, xmin, xmax):
    x_base, xerr, y_base = prep.data_loc_to_data(data_loc, xmin, xmax, 10000)
    return init_pdf_bar_dataframe(ax, x_base, xerr, y_base)

def setbar(bars, x_base, xerr, y_base):
    height = y_base
    width = [i * 2 for i in xerr]

    i = 0
    for bar in bars:
        try:
            bar.set_x(x_base[i] - width[i] / 2)
            bar.set_height(height[i])
            bar.set_width(width[i])
        except:
            bar.set_height(0)
            bar.set_width(0)
        i = i + 1

    return bar


def setbar_for_loc(bars, data_loc, xmin, xmax):
    x_base, xerr, y_base = prep.data_loc_to_data(data_loc, xmin, xmax)

    return setbar(bars, x_base, xerr, y_base)


def pdfplot_bar(ax, expected, data_loc, xmin, xmax, ymin, ymax):
    pdfframe_with_expected(ax, expected, xmin, xmax, ymin, ymax)
    bars = init_pdf_bar_dataframe_for_loc(ax, data_loc, xmin, xmax)
    setbar_for_loc(bars, data_loc, xmin, xmax)


def pdfplot_bar_wo_expected(ax, data_loc, xmin, xmax, ymin, ymax):
    pdfframe(ax, xmin, xmax, ymin, ymax)
    bars = init_pdf_bar_dataframe_for_loc(ax, data_loc, xmin, xmax)
    setbar_for_loc(bars, data_loc, xmin, xmax)


def animated_pdfplot_bar(data_loc, start, end, step, expected, xmin, xmax, ymin, ymax):
    fig, ax = plt.subplots()

    init_data_loc = data_loc(start)
    pdfframe(ax, xmin, xmax, ymin, ymax)
    lines = empty_pdf_dashed_dataframe(ax)
    bars = init_pdf_bar_dataframe_for_loc(ax, init_data_loc, xmin, xmax)

    def animate(i):
        setline_for_expected(lines, lambda x: expected(x, i), xmin, xmax)
        setbar_for_loc(bars, data_loc(i), xmin, xmax)
        ax.set_title("estimated pdf for update count: " + str(i))
        return bars, lines

    idxs = np.arange(start, end, step)
    return animation.FuncAnimation(fig, animate, idxs, repeat=False, blit=False)

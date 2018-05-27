import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import preprocessing as prep
from scipy.stats import norm


def pdfframe(ax, xmin, xmax, ymin, ymax):
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)
    ax.set_ylabel("cumulative density")
    ax.set_xlabel("x")


def empty_pdf_expected_dataframe(ax):
    return ax.plot([], [], '--', color='orange')


def setline_for_expected(lines, expected, xmin, xmax):
    margin = 10
    line, = lines
    x = np.arange(xmin - margin, xmax + margin, 0.1)
    y = expected(np.array(x).tolist())
    line.set_data(x, y)


def empty_pdf_observed_dataframe(ax):
    return ax.plot([], [])


def setline_for_observed(lines, data_loc):
    data = prep.data(data_loc, 2)
    x = list(map(lambda d: d[0], data))
    y = list(map(lambda d: d[1] , data))
    line, = lines
    line.set_data(x, y)


# Animated

def animated_cdfplot(data_loc, start, end, step, expected, xmin, xmax, ymin, ymax):
    fig, ax = plt.subplots()

    init_data_loc = data_loc(start)
    pdfframe(ax, xmin, xmax, ymin, ymax)
    expectedax = empty_pdf_expected_dataframe(ax)
    observedax = empty_pdf_observed_dataframe(ax)

    def animate(i):
        setline_for_expected(expectedax, lambda x: expected(x, i), xmin, xmax)
        setline_for_observed(observedax, data_loc(i))
        ax.set_title("estimated cdf for update count: " + str(i))
        return observedax, expectedax

    idxs = np.arange(start, end, step)
    return animation.FuncAnimation(fig, animate, idxs, repeat=False, blit=False)

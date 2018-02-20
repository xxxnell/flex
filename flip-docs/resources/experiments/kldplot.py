import preprocessing as prep

def kldplot(ax, kld_data_loc, kld_max, countmin, countmax, rearr_start, rearr_period):
    kld_data = prep.data(kld_data_loc, 2)

    x3 = list(map(lambda d: d[0], kld_data))
    y3 = list(map(lambda d: abs(d[1]) , kld_data))
    ax.plot(x3, y3)
    for i in range(rearr_start, countmax, rearr_period):
        ax.axvline(i, color='r', linestyle=':', linewidth=1)
    ax.set_ylabel("KL divergence")
    ax.set_xlabel("update count")
    ax.set_xlim(countmin, countmax)
    ax.set_ylim(0.0, kld_max)

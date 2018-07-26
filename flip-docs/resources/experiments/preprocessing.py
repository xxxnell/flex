import numpy as np
import math
import decimal

def data_str(name):
    with open(name, 'r') as f:
        content = f.readlines()

    return list(map(lambda elem: [d.strip() for d in elem.split(",")], content))


def data(name, i):
    with open(name, 'r') as f:
        content = f.readlines()

    data = np.zeros((len(content), i))
    for j, elem in enumerate(content):
        raw = [float(d.strip()) for d in elem.split(",")]
        acc_data = []
        for raw_elem in raw:
            acc_data.append(raw_elem)
        data[j] = acc_data
    return data


def transform(raws, xmin, xmax):
    raws = list(filter(lambda d: (d[0] >= xmin) & (d[0] <= xmax), raws))
    raws = list(filter(lambda d: (d[1] >= xmin) & (d[1] <= xmax), raws))

    data = np.zeros((len(raws), 3))
    for i, raw in enumerate(raws):
        x = (raw[0] + raw[1])/2
        xerr = (raw[1] - raw[0])/2
        y = raw[2]
        data[i] = [x, xerr, y]
    return data


def unzip(data):
    x1 = list(map(lambda d: d[0], data))
    x2 = list(map(lambda d: d[1], data))
    x3 = list(map(lambda d: d[2], data))
    return (x1, x2, x3)


def data_loc_to_data(data_loc, xmin, xmax, margin = 10):
    raw_dat = data(data_loc, 3)
    dat = transform(raw_dat, xmin - margin, xmax + margin)
    return unzip(dat)


def precision(x, s):
    if x != 0:
        dgt = math.floor(math.log10(abs(x)))
        round_dgt = -int(dgt - (s - 1))
        resn = round(x, round_dgt)
        if resn < 1:
            ress = ("%." + str(round_dgt) + "f") % resn
        else:
            ress = str(resn)
        return ress
    else:
        return "0." + "0"*(s-1)

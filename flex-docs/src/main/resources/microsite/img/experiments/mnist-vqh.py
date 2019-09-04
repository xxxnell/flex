import csv
import numpy as np
from matplotlib import pyplot as plt

datno = 100
dir = '../../../../../flex-bench/experiments/mnist-vqh/'

def read(loc):
    images = []
    ns = []
    with open(loc, newline='') as csvfile:
        spamreader = csv.reader(csvfile, delimiter=',', quotechar='|')
        for row in spamreader:
            vec = np.split(np.array(row, dtype='float'), [784])
            image = vec[0].reshape((28, 28))
            n = vec[1][0] if len(vec[1]) > 0 else None
            images.append(image)
            ns.append(n)
    return images, ns


imagesseq = []
nsseq = []
col = 0
inps = read(dir + 'mnist-vqh-in.out')[0][:datno]
for i in range(datno):
    images, ns = read(dir + 'mnist-vqh-vqh-' + str(i) + '.out')
    imagesseq.append(images)
    nsseq.append(ns)
    col = len(images) if len(images) > col else col

fig = plt.figure(figsize=(col + 1, datno))

for i in range(datno):
    for j in range(col + 1):
        ax = plt.subplot(datno, col + 1, i * (col + 1) + j + 1)
        if j == 0 and i > 0:
            ax.imshow(inps[i - 1])
        elif len(imagesseq[i]) > 0 and len(imagesseq[i]) > j - 1:
            ax.imshow(imagesseq[i][j - 1], cmap='gray')
            ax.text(0, -1, round(nsseq[i][j - 1], 2), color='black')
        ax.xaxis.set_visible(False)
        ax.yaxis.set_visible(False)

fig.tight_layout()
name = 'mnist-vqh'
plt.savefig(name + '.pdf')
plt.savefig(name + '.png')
plt.show()

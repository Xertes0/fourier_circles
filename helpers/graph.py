# Graphs points

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np

f = open("points.txt", "r")
_ = f.readline()

xs, ys = [], []
for line in f.readlines():
    x, y = map(float, line.split(" "))
    xs.append(x)
    ys.append(y)

fig, ax = plt.subplots()
ax.plot(xs, ys)
plt.show()

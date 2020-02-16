import math
import matplotlib.pyplot as plt
import numpy as np
age, hM, nM, eM, hF, nF, eF = np.loadtxt('SSA.txt', unpack=True)
intHM = np.cumsum(hM)
hatSM = np.exp(-intHM)
pmfM  = hM * hatSM

plt.plot(age, pmfM, 'k')

plt.savefig('SSA.pdf')
plt.show()

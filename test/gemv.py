import numpy as np

n = 1024

matrix = np.random.rand(n,n)
vector = np.random.rand(n)
np.savetxt("matrix.csv", matrix, delimiter=",")
np.savetxt("vector.csv", vector, delimiter=",")

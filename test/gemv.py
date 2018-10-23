import numpy as np

n = 1024

matrix = np.random.randint(200, size=(n,n))
vector = np.random.randint(200, size=n)
gold = np.matmul(matrix, vector)
np.savetxt("matrix.csv", matrix, fmt="%i", delimiter=",")
np.savetxt("vector.csv", vector, fmt="%i", delimiter=",")
np.savetxt("gold.csv", gold, fmt="%i", delimiter=",")



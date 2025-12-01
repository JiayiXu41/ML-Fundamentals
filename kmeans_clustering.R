# Technique: K-Means Clustering
# Author: Jiayi Xu
# Description: Performs unsupervised clustering on synthetic data.

set.seed(1)
n <- 200
x1 <- c(rnorm(n/2, -2), rnorm(n/2, 3))
x2 <- c(rnorm(n/2, 1),  rnorm(n/2, -1))
X <- cbind(x1, x2)

km <- kmeans(X, centers = 2)
print(km$centers)

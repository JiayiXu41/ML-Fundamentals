# Technique: Lasso Feature Selection
# Author: Jiayi Xu
# Description: Demonstrates L1 regularization for sparse linear signals.

library(glmnet)
set.seed(1)

n <- 100
p <- 20

X <- matrix(rnorm(n * p), n, p)
beta <- c(1, 0.7, 0.4, rep(0, p - 3))
y <- X %*% beta + rnorm(n)

cvfit <- cv.glmnet(X, y, alpha = 1)
print(coef(cvfit, s = "lambda.1se"))
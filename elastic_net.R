# Technique: Elastic Net Regression
# Author: Jiayi Xu
# Description: Combines L1 and L2 penalties for sparse + stable estimates.

library(glmnet)
set.seed(4)

n <- 150
p <- 40

X <- matrix(rnorm(n * p), n, p)
beta <- c(rep(1, 5), rep(0.3, 5), rep(0, p - 10))
y <- X %*% beta + rnorm(n)

cvfit <- cv.glmnet(X, y, alpha = 0.5)   # alpha = 0.5 is elastic net
print(coef(cvfit, s = "lambda.min"))
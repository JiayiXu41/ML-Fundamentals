# Technique: Ridge Regression
# Author: Jiayi Xu
# Description: Uses L2 regularization to stabilize coefficients in correlated settings.

library(glmnet)
set.seed(2)

n <- 120
p <- 25

X <- matrix(rnorm(n * p), n, p)
beta <- c(1.5, -0.5, rep(0, p - 2))
y <- X %*% beta + rnorm(n)

cvfit <- cv.glmnet(X, y, alpha = 0)
print(coef(cvfit, s = "lambda.min"))

# Technique: K-Nearest Neighbors Regression
# Author: Jiayi Xu
# Description: Implements KNN regression from scratch and evaluates test MSE.

set.seed(1)

# ----- KNN function -----
myknn <- function(trainx, trainy, testx, k) {
  n_test <- nrow(testx)
  preds <- numeric(n_test)
  
  for (i in 1:n_test) {
    dists <- sqrt(rowSums((trainx - testx[i, ])^2))
    nn <- order(dists)[1:k]
    preds[i] <- mean(trainy[nn])
  }
  preds
}

# ----- Simulate data -----
n_train <- 400
n_test  <- 1000

X_train <- matrix(rnorm(n_train * 3), n_train, 3)
X_test  <- matrix(rnorm(n_test * 3),  n_test, 3)

f <- function(x) 0.5*x[,1] + sin(x[,2]) - 0.3*x[,3]^2

Y_train <- f(X_train) + rnorm(n_train, 0, 0.1)
Y_test  <- f(X_test)  + rnorm(n_test, 0, 0.1)

# ----- Evaluate k = 1 to 10 -----
k_grid <- 1:10
mse <- numeric(10)

for (i in k_grid) {
  pred <- myknn(X_train, Y_train, X_test, i)
  mse[i] <- mean((pred - Y_test)^2)
}

print(data.frame(k = k_grid, Test_MSE = mse))
